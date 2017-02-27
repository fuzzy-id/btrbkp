module Btrbkp.FrontEnd.Snapshots (snapshotsFrontEnd) where

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ini (Ini())
import Data.Text (Text(), pack, unpack)
import System.FilePath (FilePath())
import System.Linux.Btrfs (getSubvol, resolveSubvol)

import Btrbkp.Logging
import Btrbkp.Config.Query
import Btrbkp.Types
import Btrbkp.BkpStore.Local

data SnapshotsConfig = SnapshotsConfig { sources :: [Text] }

snapshotsFrontEnd :: FrontEndModule SnapshotsConfig
snapshotsFrontEnd = FrontEndModule frontEndName readSources mkBkpPlan

readSources :: Ini -> Text -> Either String SnapshotsConfig
readSources cfg sect = SnapshotsConfig <$> queryList cfg sect (pack "sources") 

frontEndName :: Text
frontEndName = pack "snapshots"

btrbkp :: MonadIO m => FilePath -> Btrbkp m
btrbkp src = do bRoot <- view backupRoot
                nameInFs <- liftIO (resolveSubvol bRoot =<< getSubvol src)
                backupSnapshot frontEndName src nameInFs

mkBkpPlan :: MonadIO m => SnapshotsConfig -> m [Btrbkp m]
mkBkpPlan = return . map (btrbkp . unpack) . sources
