{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-|
TODO:
    * Logging errors & normal output to files
    * Config file parsing
-}
module HsBackup
    ( run
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
import           Data.Maybe                     ( catMaybes
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
import           GHC.Generics                   ( Generic )
import           System.Process.Typed           ( proc
                                                , setStdin
                                                , runProcess
                                                , closed
                                                )
import           System.Directory               ( listDirectory
                                                , removePathForcibly
                                                , doesFileExist
                                                )
import           System.FilePath.Posix          ( (</>) )
import           System.Exit                    ( ExitCode(..) )

import qualified Data.ByteString               as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T


-- Read config file instead of passing as args
run :: [Backup] -> FilePath -> FilePath -> IO ()
run backups basePath stateFile = do
    state <- initializeState backups stateFile
    let cfg = Config backups basePath stateFile state
    syncer   <- async $ runReaderT syncBackups cfg
    enqueuer <- async $ runReaderT enqueueBackups cfg
    void $ waitBoth syncer enqueuer


-- | Configuration data for the program.
data Config
    = Config
        { cfgBackups :: [Backup]
        , cfgBackupBaseDir :: FilePath
        , cfgStateFile :: FilePath
        , cfgState :: TVar AppState
        }

-- | The current status of the application - either Syncing or Synced.
data AppState
    = Synced ZonedTime
    | SyncInProgress SyncState
    deriving (Show, Read, Generic, Serialize)

-- | Try to read the statefile. If an error occurs while decoding the file,
-- sync all the backups available.
--
-- TODO: Remove enqueued backups no longer in [Backup] from state?
initializeState :: MonadIO m => [Backup] -> FilePath -> m (TVar AppState)
initializeState backups stateFile = do
    stateExists <- liftIO $ doesFileExist stateFile
    if stateExists
        then liftIO (BS.readFile stateFile) >>= \c -> case decode c of
            Left err -> do
                liftIO $ putStrLn $ "Error Decoding Application State: " <> err
                makeInitialState
            Right st -> liftIO $ newTVarIO st
        else makeInitialState
  where
    makeInitialState :: MonadIO m => m (TVar AppState)
    makeInitialState = do
        time <- liftIO getZonedTime
        let s = SyncInProgress $ SyncState time $ getAllBackups backups
        liftIO . BS.writeFile stateFile $ encode s
        liftIO $ newTVarIO s


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
        then map (, BackupYearly) backups
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
        -- TODO: Non-Empty queue only?
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
        liftIO $ putStrLn $ "IO Exception: " <> show err
        liftIO $ putStrLn "Retrying in 30 seconds."
        liftIO $ threadDelay $ 1000000 * 30
        runBackupUntilComplete a time
    Right _ ->
        liftIO
            $  putStrLn
            $  "Backup Completed: "
            <> T.unpack (bName b)
            <> " - "
            <> rateToFolderName br


-- Running a single backup
-- TODO: Run monthly & yearly by `cp -alr` on Daily or Hourly.
runBackup
    :: (MonadReader Config m, MonadIO m, MonadCatch m)
    => Backup
    -> ZonedTime
    -> BackupRate
    -> m (Either IOException FilePath)
runBackup backup time rate = try $ do
    backupPath <- rsyncWithRetry
    touchWithRetry backupPath
    deleteOverflow backup rate
    return backupPath
  where
    rsyncWithRetry :: (MonadReader Config m, MonadIO m) => m FilePath
    rsyncWithRetry =
        untilSuccess "rsync" (getBackupPath (bName backup) rate time)
            $ runRsync backup time rate
    touchWithRetry :: MonadIO m => FilePath -> m ()
    touchWithRetry = untilSuccess "touch" (return ()) . updateModifiedTime
    untilSuccess :: MonadIO m => String -> m a -> m ExitCode -> m a
    untilSuccess name postAction runner = runner >>= \case
        ExitSuccess      -> postAction
        ExitFailure code -> do
            liftIO $ putStrLn $ name <> " exited with code: " <> show code
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
    backups    <- liftIO $ listDirectory parentPath
    let lastBackup = listToMaybe backups
    when (length backups > maximumBackups rate) $ do
        sequence_ (deletePath <$> lastBackup)
        deleteOverflow backup rate

-- | Forcible remove the given path.
deletePath :: MonadIO m => FilePath -> m ()
deletePath = liftIO . removePathForcibly


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
    maybeLinkDest <- getLinkDestination backup rate
    path          <- getBackupPath bName rate time
    let
        rsync = proc "rsync" $ catMaybes
            [ pure "-ahz"
            , pure "--delete"
            , (\bwl -> "--bwlimit=" <> show bwl) <$> bBandwidthLimit
            , ("--link-dest=" <>) <$> maybeLinkDest
            , pure $ "-e '" <> sshOptions backup <> "'"
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
    -> BackupRate
    -> m (Maybe FilePath)
getLinkDestination backup rate = do
    parentPath <- getParentPath backup rate
    siblings   <- liftIO $ listDirectory parentPath
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
