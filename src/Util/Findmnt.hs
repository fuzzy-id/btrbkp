module Util.Findmnt where

import Data.Bifunctor (first)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Linux.Btrfs.UUID (UUID(), fromString)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

findmnt :: MonadIO m => FilePath -> m (Either String UUID)
findmnt t = do (code,out,err) <- liftIO (readProcessWithExitCode "findmnt" args "")
               (return . prefixError) (checkResult code out err)
  where
    args = ["--output", "UUID", "--target", t]
    prefixError = first (("findmnt " ++ t ++ ": ") ++)

checkResult :: ExitCode -> String -> String -> Either String UUID
checkResult ExitSuccess out "" =
    case mUuid of
      Nothing -> Left ("Parsing uuid: " ++ uuidStr)
      Just uuid -> Right uuid
  where
    [_, uuidStr] = lines out
    mUuid = fromString uuidStr 
checkResult _           out err =
    Left err
