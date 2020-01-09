{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , name
                                                , explicit
                                                , typ
                                                , help
                                                , details
                                                , summary
                                                , program
                                                , helpArg
                                                )

import           HsBackup                       ( readConfigFile
                                                , run
                                                )
import           Paths_hs_backup                ( version )

main :: IO ()
main = do
    args   <- cmdArgs argsSpec
    config <- readConfigFile $ maybe ["/etc/hs-backup.yaml"] (: []) $ configFile
        args
    run config

data Args =
    Args
        { configFile :: Maybe FilePath
        } deriving (Show, Data, Typeable)

argsSpec :: Args
argsSpec =
    Args
            { configFile = Nothing
                           &= explicit
                           &= name "config-file"
                           &= name "c"
                           &= typ "CONFIG"
                           &= help "Path to the config file"
            }
        &= summary
               ("HsBackup " ++ showVersion version ++ ", (c) Pavan Rikhi 2019")
        &= program "hs-backup"
        &= helpArg [name "h"]
        &= details
               [ "HsBackup is a backup service that leverages rsync & hardlinks "
               <> "to make hourly, daily, monthly, & yearly backups of folders on "
               <> "remote systems."
               , ""
               , "rsync is used to pull the smallest incremental backup. Hardlinks "
               <> "allow us to archive those backups into longer-span backups while "
               <> "conserving harddisk space. If a network failure occurs, HsBackup "
               <> "will retry syncing of the backup until it has completed."
               , ""
               , "Old backups will automatically be cleaned up after a certain amount "
               <> "of time. Hourly backups are kept for 24 Hours, Daily for 31 "
               <> "Days, Monthly for 1 Year, & Yearly backups are kept forever."
               , ""
               , "Configuration is done via a YAML-file. You can pass the path to the "
               <> "file using the `--config-file` flag. Without the flag, HsBackup "
               <> "will attempt to read a configuration file at `/etc/hs-backup.yaml`."
               ]
