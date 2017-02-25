module Btrbkp.FrontEnd.Snapshots (snapshotsFrontEnd) where

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text as P
import Data.Ini (Ini(..), lookupValue, parseValue, sections)
import Data.Text (Text(), pack, unpack)
import System.FilePath (FilePath())
import System.Linux.Btrfs (getSubvol, resolveSubvol, snapshot)

import Btrbkp.Logging
import Btrbkp.Types

data SnapshotsConfig = SnapshotsConfig { sources     :: [Text]
                                       }
                                       deriving (Eq,Show)

snapshotsFrontEnd :: FrontEndModule SnapshotsConfig
snapshotsFrontEnd = FrontEndModule frontEndName readSources mkBkpPlan

frontEndName = pack "snapshots"

readSources :: Ini -> Text -> Either String SnapshotsConfig
readSources ini sect =
    SnapshotsConfig <$> parseValue sect (pack "sources") listP ini

listP :: P.Parser [Text]
listP =
    P.manyTill (P.takeWhile (/= listSep) <* P.skipWhile (== listSep)) P.endOfInput
  where
    listSep = ','

btrbkp :: MonadIO m => FilePath -> Btrbkp m
btrbkp src = do bRoot <- view backupRoot
                nameInFs <- liftIO (resolveSubvol bRoot =<< getSubvol src)
                dest <- view destinationBuilder <*> pure frontEndName <*> pure nameInFs
                bkpLog Info (src `msgTo` dest)
                liftIO (snapshot src dest True)
                bkpLog Info (msg "Done")

mkBkpPlan :: MonadIO m => SnapshotsConfig -> m [Btrbkp m]
mkBkpPlan = return . map (btrbkp . unpack) . sources
