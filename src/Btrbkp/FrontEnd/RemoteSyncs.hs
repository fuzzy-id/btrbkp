module Btrbkp.FrontEnd.RemoteSyncs (remoteSyncsFrontend) where

import Control.Lens (view)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ini (Ini(), lookupValue)
import Data.Text (Text(), pack, unpack)
import System.Directory (doesDirectoryExist)
import System.Linux.Btrfs (createSubvol)

import Btrbkp.BkpStore.Local
import Btrbkp.Config.Query
import Btrbkp.Types

data RemoteSyncsConfig = RemoteSyncsConfig { sources   :: [Text]
                                           , remote    :: Text
                                           , port      :: Int
                                           , excludes  :: [Text]
                                           }

remoteSyncsFrontend :: FrontEndModule RemoteSyncsConfig
remoteSyncsFrontend = FrontEndModule frontEndName readConfig mkBkpPlan

readConfig :: Ini -> Text -> Either String RemoteSyncsConfig
readConfig cfg sect =
    RemoteSyncsConfig <$> queryList cfg sect (pack "sources")
                      <*> lookupValue sect (pack "remote") cfg
                      <*> queryIntWithDefault 22 cfg sect (pack "port")
                      <*> queryList cfg sect (pack "excludes")

frontEndName :: Text
frontEndName = pack "remote-syncs"

remoteSync :: MonadIO m => String -> Int -> [String] -> FilePath -> Btrbkp m
remoteSync host port args path =
    do sep <- view separator
       let normalized = host ++ sep ++ normalize path
       syncDir <- syncDirBuilder frontEndName normalized
       syncDirExists <- (liftIO . doesDirectoryExist) syncDir
       unless syncDirExists (liftIO (createSubvol syncDir))
       rsyncRemote host port path syncDir args
       backupSnapshot frontEndName syncDir normalized

mkBkpPlan :: MonadIO m => RemoteSyncsConfig -> m [Btrbkp m]
mkBkpPlan cfg = (return 
                 . map (remoteSync host (port cfg) args . unpack) 
                 . sources
                 ) cfg
  where
    args = map (("--exclude=" ++) . unpack) (excludes cfg)
    host = (unpack . remote) cfg
