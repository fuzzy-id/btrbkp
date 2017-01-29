module Util.Findmnt where

import Data.Bifunctor (first)
import Data.UUID (UUID(), fromString)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

findmnt :: FilePath -> IO (Either String UUID)
findmnt t = do (code,out,err) <- readProcessWithExitCode "findmnt" args ""
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
