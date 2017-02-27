module Btrbkp.FrontEnd.LocalSyncs (localSyncsFrontend) where

import Control.Lens (view)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ini (Ini())
import Data.Text (Text(), pack, unpack)
import System.Directory (doesDirectoryExist)
import System.Linux.Btrfs (createSubvol)

import Btrbkp.Config.Query
import Btrbkp.Logging
import Btrbkp.Types
import Btrbkp.BkpStore.Local

data LocalSyncsConfig = LocalSyncsConfig { sources :: [Text] }

localSyncsFrontend :: FrontEndModule LocalSyncsConfig
localSyncsFrontend = FrontEndModule frontEndName readSources mkBkpPlan

readSources :: Ini -> Text -> Either String LocalSyncsConfig
readSources cfg sect = LocalSyncsConfig <$> queryList cfg sect (pack "sources")

frontEndName :: Text
frontEndName = pack "local-syncs"

localSync :: MonadIO m => FilePath -> Btrbkp m
localSync src = do syncDir <- syncDirBuilder frontEndName normalized
                   syncDirExists <- (liftIO . doesDirectoryExist) syncDir
                   unless syncDirExists (liftIO (createSubvol syncDir))
                   rsync src syncDir
                   backupSnapshot frontEndName syncDir normalized
  where
    normalized = normalize src

mkBkpPlan :: MonadIO m => LocalSyncsConfig -> m [Btrbkp m]
mkBkpPlan = return . map (localSync . unpack) . sources
