{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-|
TODO:
    * Cleanly handle SIGTERM - persist state, stop backup & child threads
    * More configuration options:
        * Backup folder format strings
        * BackupRate folder names
        * Maximum backups to retain
        * Delay time for checking for backups
    * Log stderr/stdout of command when they error out

-}
module HsBackup
    ( readConfigFile
    , run
    , ConfigSpec(..)
    , Config(..)
    , Backup(..)
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async.Lifted
                                                ( async
                                                , waitBoth
                                                )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , stateTVar
                                                , newTVarIO
                                                , readTVarIO
                                                )
import           Control.Monad                  ( when
                                                , void
                                                , sequence_
                                                , forever
                                                , forM_
                                                )
import           Control.Exception.Safe         ( MonadCatch
                                                , IOException
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , runReaderT
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Data.Serialize                 ( Serialize
                                                , encode
                                                , decode
                                                )
import           Data.Serialize.Text            ( )
import           Data.Time                      ( ZonedTime(..)
                                                , LocalTime(..)
                                                , TimeOfDay(..)
                                                , toGregorian
                                                , getZonedTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Time.LocalTime.Serialize  ( )
import           Data.Yaml.Config               ( loadYamlSettings
                                                , ignoreEnv
                                                )
import           GHC.Generics                   ( Generic )
import           System.Process.Typed           ( proc
                                                , setStdin
                                                , setStdout
                                                , runProcess
                                                , closed
                                                )
import           System.Directory               ( listDirectory
                                                , removePathForcibly
                                                , doesFileExist
                                                , doesPathExist
                                                , getPermissions
                                                , Permissions
                                                , writable
                                                , readable
                                                , getTemporaryDirectory
                                                , createDirectoryIfMissing
                                                )
import           System.Exit                    ( ExitCode(..) )
import           System.FilePath.Posix          ( (</>)
                                                , takeDirectory
                                                )
import           System.Log.FastLogger          ( TimedFastLogger
                                                , LogStr
                                                , toLogStr
                                                , FormattedTime
                                                , newTimeCache
                                                , FileLogSpec(..)
                                                , LogType(..)
                                                , newTimedFastLogger
                                                , defaultBufSize
                                                )

import qualified Data.ByteString               as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- | Parse the program configuration from the given paths.
readConfigFile :: [FilePath] -> IO ConfigSpec
readConfigFile paths = loadYamlSettings paths [] ignoreEnv

-- | The configuration data parsed from the configuratin file.
data ConfigSpec =
    ConfigSpec
        { csBackups :: [Backup]
        , csBackupPath :: FilePath
        , csStatePath :: FilePath
        , csLogFile :: Maybe FilePath
        }

instance FromJSON ConfigSpec where
    parseJSON = withObject "ConfigSpec" $ \v -> do
        csBackups    <- v .: "backups"
        csBackupPath <- v .: "backup-folder"
        csStatePath  <- v .: "state-file"
        csLogFile    <- v .: "log-file"
        return ConfigSpec { .. }


-- | Start a thread that enqueues new Backups and a thread that processes
-- the queue by syncing the backups.
--
-- Read config file instead of passing as args
run :: ConfigSpec -> IO ()
run spec@ConfigSpec { csBackups, csBackupPath, csStatePath } = do
    (logger, cleanupLogger) <- makeLogger
    createBackupFolders
    statePath <- checkStatePath logger
    state     <- initializeState csBackups statePath logger
    let cfg = Config csBackups csBackupPath statePath state logger
    syncer   <- async $ runReaderT syncBackups cfg
    enqueuer <- async $ runReaderT enqueueBackups cfg
    void $ waitBoth syncer enqueuer
    cleanupLogger
  where
    -- Build the application logger. If no path exists in the
    -- configuration, log to stdout. If the path exists but we cannot write
    -- to it, log to stdout. Otherwise use the specified path for the log
    -- output.
    makeLogger :: IO (TimedFastLogger, IO ())
    makeLogger = do
        timeCache <- newTimeCache "%Y-%m-%dT%T"
        newTimedFastLogger timeCache =<< case csLogFile spec of
            Nothing       -> return $ LogStdout defaultBufSize
            Just filePath -> do
                permissions <- getFilePermissions filePath
                if writable permissions
                    then return $ LogFile
                        (FileLogSpec filePath (1024 * 1024 * 50) 5)
                        defaultBufSize
                    else do
                        putStrLn
                            $  "Warning: Can not write to log file at "
                            <> filePath
                            <> ", logging to stdout instead"
                        return $ LogStdout defaultBufSize
    -- Ensure all the @<backup>/<rate>@ folders are created.
    createBackupFolders :: IO ()
    createBackupFolders = do
        let backups = getAllBackups csBackups
        forM_ backups $ \(backup, rate) ->
            createDirectoryIfMissing True
                $   csBackupPath
                </> T.unpack (bName backup)
                </> rateToFolderName rate
    -- Check the read & write permissions for the state file, issuing
    -- a warning & using a temporary file if we cannot read or write to the
    -- given path.
    checkStatePath :: TimedFastLogger -> IO FilePath
    checkStatePath logger = do
        permissions <- liftIO $ getFilePermissions csStatePath
        if writable permissions && readable permissions
            then do
                createDirectoryIfMissing True $ takeDirectory csStatePath
                return csStatePath
            else do
                logger
                    .  logMsg_
                    .  toLogStr
                    $  "Warning: Could not read/write state file at "
                    <> csStatePath
                    <> ", backup status will not be persisted across program restarts."
                tempDir <- getTemporaryDirectory
                return $ tempDir </> "hs-backup.state"
    -- Try fetching the file permissions for a given path, recursively
    -- ascending the directory tree until we find a path that exists.
    getFilePermissions :: FilePath -> IO Permissions
    getFilePermissions filePath = do
        exists <- doesPathExist filePath
        if exists
            then getPermissions filePath
            else getFilePermissions $ takeDirectory filePath


-- | The configuration environment used by the enqueuing & syncing threads.
data Config
    = Config
        { cfgBackups :: [Backup]
        , cfgBackupBaseDir :: FilePath
        , cfgStateFile :: FilePath
        , cfgState :: TVar AppState
        , cfgLogger :: TimedFastLogger
        }

-- | Log a message to the log file.
logMsg :: (MonadReader Config m, MonadIO m) => LogStr -> m ()
logMsg msg = do
    logger <- asks cfgLogger
    liftIO $ logger $ logMsg_ msg

-- | Helper for building log strings. Used by 'logMsg' but can also be used
-- to log messages in non-MonadReader contexts.
logMsg_ :: LogStr -> FormattedTime -> LogStr
logMsg_ msg time = "[" <> toLogStr time <> "]: " <> msg <> "\n"

-- | The current status of the application - either Syncing or Synced.
data AppState
    = Synced ZonedTime
    | SyncInProgress SyncState
    deriving (Show, Read, Generic, Serialize)

-- | Try to read the statefile. If an error occurs while decoding the file,
-- sync all the backups available.
initializeState
    :: MonadIO m => [Backup] -> FilePath -> TimedFastLogger -> m (TVar AppState)
initializeState backups stateFile logger = do
    liftIO . logger $ logMsg_ "Initializing Application State"
    stateExists <- liftIO $ doesFileExist stateFile
    if stateExists
        then liftIO (BS.readFile stateFile) >>= \c -> case decode c of
            Left err -> do
                liftIO
                    .  logger
                    .  logMsg_
                    .  toLogStr
                    $  "Error Decoding Application State: "
                    <> err
                makeInitialState
            Right st -> liftIO $ do
                logger $ logMsg_ "Initialized State From File"
                newTVarIO $ removeOldBackups st
        else makeInitialState
  where
    -- Build & save an initial AppState from the Backups.
    makeInitialState :: MonadIO m => m (TVar AppState)
    makeInitialState = liftIO $ do
        time <- getZonedTime
        let s = SyncInProgress $ SyncState time $ getAllBackups backups
        BS.writeFile stateFile $ encode s
        logger $ logMsg_ "Created New Application State"
        newTVarIO s
    -- Remove any enqueued Backups that are no longer in the list of
    -- Backups to make.
    removeOldBackups :: AppState -> AppState
    removeOldBackups = \case
        s@(Synced _)             -> s
        SyncInProgress syncState -> SyncInProgress $ syncState
            { ssQueue = filter (\(backup, _) -> backup `elem` backups)
                            $ ssQueue syncState
            }


-- | Sync the next queued backup or wait 5 minutes if the queue if empty.
syncBackups :: (MonadReader Config m, MonadIO m, MonadCatch m) => m ()
syncBackups = forever $ do
    appState <- liftIO . readTVarIO =<< asks cfgState
    case appState of
        Synced         _         -> liftIO . threadDelay $ 1000000 * 60 * 5
        SyncInProgress syncState -> case listToMaybe (ssQueue syncState) of
            Nothing -> do
                time <- liftIO getZonedTime
                void . updateState $ const $ Synced time
            Just nextBackup -> do
                runBackupUntilComplete nextBackup $ ssStartTime syncState
                time <- liftIO getZonedTime
                void . updateState $ \case
                    s@(Synced _)       -> s
                    SyncInProgress ss_ -> SyncInProgress ss_
                        { ssQueue     = drop 1 $ ssQueue ss_
                        , ssStartTime = time
                        }


-- | Add more backup jobs if it is time to do so. If not, sleep for
-- 5 minutes.
enqueueBackups :: (MonadReader Config m, MonadIO m) => m ()
enqueueBackups = forever $ do
    backups <- asks cfgBackups
    time    <- liftIO getZonedTime
    void . updateState $ \case
        Synced lastSyncTime ->
            let backupsDue = calculateBackups backups lastSyncTime time
            in  if null backupsDue
                    then Synced time
                    else SyncInProgress $ SyncState { ssStartTime = time
                                                    , ssQueue     = backupsDue
                                                    }
        SyncInProgress ss ->
            let backupsDue = calculateBackups backups (ssStartTime ss) time
            in  SyncInProgress ss
                    { ssQueue = ssQueue ss <> (backupsDue L.\\ ssQueue ss)
                    }
    liftIO . threadDelay $ 1000000 * 60 * 5


-- | Check the time difference and see what backups are due.
calculateBackups :: [Backup] -> ZonedTime -> ZonedTime -> [(Backup, BackupRate)]
calculateBackups backups lastTime newTime = concat
    [hourly, daily, monthly, yearly]
  where
    zonedHour = todHour . localTimeOfDay . zonedTimeToLocalTime
    zonedDay =
        (\(_, _, d) -> d) . toGregorian . localDay . zonedTimeToLocalTime
    zonedMonth =
        (\(_, m, _) -> m) . toGregorian . localDay . zonedTimeToLocalTime
    hourChange = zonedHour lastTime /= zonedHour newTime
    dayChange  = zonedDay lastTime /= zonedDay newTime
    hourly     = if hourChange
        then map (, BackupHourly) $ filter bEnableHourly backups
        else []
    daily   = if dayChange then map (, BackupDaily) backups else []
    monthly = if dayChange && zonedDay newTime == 1
        then map (, BackupMonthly) backups
        else []
    yearly = if dayChange && zonedDay newTime == 1 && zonedMonth newTime == 1
        then map (, BackupYearly) $ filter bEnableYearly backups
        else []


-- | Update the state, persisting changes to the State file.
updateState
    :: (MonadReader Config m, MonadIO m) => (AppState -> AppState) -> m AppState
updateState modifier = do
    state    <- asks cfgState
    newState <-
        liftIO . atomically $ stateTVar state $ (\a -> (a, a)) . modifier
    stateFile <- asks cfgStateFile
    liftIO $ BS.writeFile stateFile $ encode newState
    return newState


-- | Data retained when syncing is in progress.
data SyncState
    = SyncState
        { ssStartTime :: ZonedTime
        -- ^ The time the current Backup was initialized
        , ssQueue :: [(Backup, BackupRate)]
        -- ^ A queue of backups to make. The first item is the currently
        -- synced one.
        -- TODO: Non-Empty queue only? How would that work when we've
        -- finished processing?
        } deriving (Show, Read, Generic, Serialize)

-- | Data required for making a backup of a remote location.
data Backup
    = Backup
        { bName :: T.Text
        -- ^ Name / Identifier of the backup. Used in loggging & as base
        -- folder name.
        , bServer :: T.Text
        -- ^ The remote server to connect to.
        , bUser :: T.Text
        -- ^ The user to SSH into the server as.
        , bPath :: FilePath
        -- ^ The remote path to the backup location.
        , bIdentityFile :: FilePath
        -- ^ Path to the SSH key to use when logging in.
        , bEnableHourly :: Bool
        -- ^ Enable hourly backups.
        , bEnableYearly :: Bool
        -- ^ Enable yearly backups.
        , bBandwidthLimit :: Maybe Integer
        -- ^ Limit the bandwidth used to make backups.
        } deriving (Show, Read, Eq, Generic, Serialize)

instance FromJSON Backup where
    parseJSON = withObject "Backup" $ \v -> do
        bName           <- v .: "name"
        bServer         <- v .: "server"
        bUser           <- v .: "user"
        bPath           <- v .: "path"
        bIdentityFile   <- v .: "identity-file"
        bEnableHourly   <- fromMaybe False <$> v .:? "hourly"
        bEnableYearly   <- fromMaybe False <$> v .:? "yearly"
        bBandwidthLimit <- v .:? "bandwidth-limit"
        return Backup { .. }


-- | The different rates at which backups are taken.
data BackupRate
    = BackupHourly
    | BackupDaily
    | BackupMonthly
    | BackupYearly
    deriving (Show, Read, Eq, Ord, Generic, Serialize)

-- | Parent folder name for each BackupRate.
rateToFolderName :: BackupRate -> FilePath
rateToFolderName = \case
    BackupHourly  -> "Hourly"
    BackupDaily   -> "Daily"
    BackupMonthly -> "Monthly"
    BackupYearly  -> "Yearly"

-- | Number of backups to retain for each BackupRate.
maximumBackups :: BackupRate -> Int
maximumBackups = \case
    BackupHourly  -> 24
    BackupDaily   -> 31
    BackupMonthly -> 12
    BackupYearly  -> 2000

-- | The time formatting string for the folder name of a BackupRate.
formatString :: BackupRate -> String
formatString = \case
    BackupHourly  -> "%F_%R"
    BackupDaily   -> "%F"
    BackupMonthly -> "%F"
    BackupYearly  -> "%F"

-- | Path like "Daily/2019-02-01"
getDateFolder :: ZonedTime -> BackupRate -> FilePath
getDateFolder time rate = (rateToFolderName rate </>)
    $ formatTime defaultTimeLocale (formatString rate) time

-- | Build a list of all possible backup jobs, ordered by the BackupRate.
getAllBackups :: [Backup] -> [(Backup, BackupRate)]
getAllBackups = L.sortOn snd . concatMap makePairs
  where
    makePairs :: Backup -> [(Backup, BackupRate)]
    makePairs b@Backup {..} = map (b, ) $ catMaybes
        [ if bEnableHourly then pure BackupHourly else Nothing
        , pure BackupDaily
        , pure BackupMonthly
        , if bEnableYearly then pure BackupYearly else Nothing
        ]

-- | Run all the backups until they don't return IO Exceptions.
runBackupUntilComplete
    :: (MonadReader Config m, MonadIO m, MonadCatch m)
    => (Backup, BackupRate)
    -> ZonedTime
    -> m ()
runBackupUntilComplete a@(b, br) time = runBackup b time br >>= \case
    Left err -> do
        logMsg
            .  toLogStr
            $  "Encountered IO Exception for "
            <> T.unpack (bName b)
            <> " "
            <> rateToFolderName br
            <> ": "
            <> show err
        logMsg "Retrying in 30 Seconds"
        liftIO $ threadDelay $ 1000000 * 30
        runBackupUntilComplete a time
    Right _ ->
        logMsg
            .  toLogStr
            $  "Backup of "
            <> T.unpack (bName b)
            <> " "
            <> rateToFolderName br
            <> " Completed Successfully"


-- Running a single backup
runBackup
    :: (MonadReader Config m, MonadIO m, MonadCatch m)
    => Backup
    -> ZonedTime
    -> BackupRate
    -> m (Either IOException FilePath)
runBackup backup time rate = try $ do
    logMsg
        .  toLogStr
        $  "Starting Backup of "
        <> T.unpack (bName backup)
        <> " - "
        <> rateToFolderName rate
    backupPath <- makeBackup
    touchWithRetry backupPath
    deleteOverflow backup rate
    return backupPath
  where
    -- Copy backups if an existing backup from a different rate is
    -- available, otherwise make a backup with rsync.
    makeBackup :: (MonadReader Config m, MonadIO m) => m FilePath
    makeBackup = case rate of
        BackupHourly -> rsyncWithRetry
        BackupDaily  -> if bEnableHourly backup
            then cpIfBackupAvailable BackupHourly
            else rsyncWithRetry
        BackupMonthly -> cpIfBackupAvailable BackupDaily
        BackupYearly  -> cpIfBackupAvailable BackupMonthly
    cpIfBackupAvailable
        :: (MonadReader Config m, MonadIO m) => BackupRate -> m FilePath
    cpIfBackupAvailable sourceRate = do
        sourceParentPath <- getParentPath backup sourceRate
        sourceFolders    <- liftIO $ listDirectory sourceParentPath
        let lastSourceFolder = listToMaybe . L.reverse $ L.sort sourceFolders
        case lastSourceFolder of
            Nothing         -> rsyncWithRetry
            Just folderName -> cpWithRetry $ sourceParentPath </> folderName
    rsyncWithRetry :: (MonadReader Config m, MonadIO m) => m FilePath
    rsyncWithRetry =
        untilSuccess "rsync" (getBackupPath (bName backup) rate time)
            $ runRsync backup time rate
    cpWithRetry :: (MonadReader Config m, MonadIO m) => FilePath -> m FilePath
    cpWithRetry source =
        untilSuccess "cp" (getBackupPath (bName backup) rate time)
            $ runCp source backup time rate
    touchWithRetry :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
    touchWithRetry = untilSuccess "touch" (return ()) . updateModifiedTime
    untilSuccess
        :: (MonadReader Config m, MonadIO m)
        => String
        -> m a
        -> m ExitCode
        -> m a
    untilSuccess name postAction runner = runner >>= \case
        ExitSuccess      -> postAction
        ExitFailure code -> do
            logMsg
                .  toLogStr
                $  "While syncing "
                <> T.unpack (bName backup)
                <> " "
                <> rateToFolderName rate
                <> ", "
                <> name
                <> " exited with code: "
                <> show code
            logMsg "Retrying in 30 Seconds"
            liftIO $ threadDelay $ 1000000 * 30
            untilSuccess name postAction runner

-- | Set the modified time of a path to now by running the @touch@ command
-- on it.
updateModifiedTime :: MonadIO m => FilePath -> m ExitCode
updateModifiedTime path =
    let touch = proc "touch" [path] in runProcess $ setStdin closed touch

-- | Delete older backups if the retention count for the BackupRate is
-- exceeded.
deleteOverflow
    :: (MonadReader Config m, MonadIO m) => Backup -> BackupRate -> m ()
deleteOverflow backup rate = do
    parentPath <- getParentPath backup rate
    backups    <- liftIO $ L.sort <$> listDirectory parentPath
    let lastBackup = listToMaybe backups
    when (length backups > maximumBackups rate) $ do
        sequence_ (deletePath <$> lastBackup)
        deleteOverflow backup rate

-- | Forcible remove the given path.
deletePath :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
deletePath path = do
    logMsg . toLogStr $ "Removing Old Backup: " <> path
    liftIO $ removePathForcibly path


-- | Make a backup using a source directory & the @cp@ command.
--
-- The @-l@ flag will be passed to @cp@, which creates hardlinks between
-- files instead of copying them in order to conserve disk space.
runCp
    :: (MonadReader Config m, MonadIO m)
    => FilePath
    -> Backup
    -> ZonedTime
    -> BackupRate
    -> m ExitCode
runCp sourceDirectory Backup {..} time rate = do
    backupDestination <- getBackupPath bName rate time
    let cp = proc "cp" ["-al", sourceDirectory, backupDestination]
    runProcess $ setStdin closed cp


-- | Make a backup using the @rsync@ command.
--
-- The last backup for the BackupRate will be used as the linking
-- destination if one exists.
runRsync
    :: (MonadReader Config m, MonadIO m)
    => Backup
    -> ZonedTime
    -> BackupRate
    -> m ExitCode
runRsync backup@Backup {..} time rate = do
    maybeLinkDest <- getLinkDestination backup time rate
    path          <- getBackupPath bName rate time
    let
        rsync = proc "rsync" $ catMaybes
            [ pure "-ahz"
            , pure "--delete"
            , (\bwl -> "--bwlimit=" <> show bwl) <$> bBandwidthLimit
            , ("--link-dest=" <>) <$> maybeLinkDest
            , pure "-e"
            , pure $ sshOptions backup
            , pure $ T.unpack bUser <> "@" <> T.unpack bServer <> ":" <> bPath
            , pure path
            ]
    runProcess $ setStdin closed rsync

-- | Get the required SSH options for a Backup.
sshOptions :: Backup -> String
sshOptions Backup {..} = "ssh -i " <> bIdentityFile <> " -l " <> T.unpack bUser


-- | Get the last backup folder for a Backup & BackupRate. Used with the
-- @link-dest@ rsync flag.
getLinkDestination
    :: (MonadReader Config m, MonadIO m)
    => Backup
    -> ZonedTime
    -> BackupRate
    -> m (Maybe FilePath)
getLinkDestination backup time rate = do
    parentPath <- getParentPath backup rate
    let backupFolder = formatTime defaultTimeLocale (formatString rate) time
    siblings <- liftIO $ filter (/= backupFolder) <$> listDirectory parentPath
    return . listToMaybe . L.reverse $ L.sort siblings

-- | Get the parent path for a specific Backup & BackupRate. E.g.,
-- @/<base-backup-path>/Community/Daily/@.
getParentPath :: MonadReader Config m => Backup -> BackupRate -> m FilePath
getParentPath backup rate = do
    base <- asks cfgBackupBaseDir
    return $ base </> T.unpack (bName backup) </> rateToFolderName rate


-- | Get the full path for a specific Backup & BackupRate.
-- E.g., @/<base-backup-path>/Community/Daily/2019-10-08/@.
getBackupPath
    :: MonadReader Config m => T.Text -> BackupRate -> ZonedTime -> m FilePath
getBackupPath backupName rate startTime = do
    base <- asks cfgBackupBaseDir
    let dateFolderName = getDateFolder startTime rate
    return $ base </> T.unpack backupName </> dateFolderName
