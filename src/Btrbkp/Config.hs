{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Btrbkp.Config (createEnv) where

import Data.Bifunctor (first)
import Data.Hourglass (timePrint)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (empty, fromList)
import Data.Ini (Ini(..), lookupValue, parseValue, sections)
import Data.Text (Text(), pack, unpack)
import System.FilePath ((</>), isPathSeparator, splitDirectories)
import System.Logger as L
import System.Logger (Logger(), )
import Time.System (dateCurrent)

import Btrbkp.Types
import Btrbkp.Logging

createEnv :: MonadIO m => Ini -> m BtrbkpEnv
createEnv cfg = BtrbkpEnv <$> createLogger cfg
                          <*> pure (createBackupRoot cfg)
                          <*> createTimestamp cfg
                          <*> pure (createSeparator cfg)

createLogger :: MonadIO m => Ini -> m Logger
createLogger cfg = (new . setOutput output . setLogLevel level) defSettings
  where
    output = mapOutput $ lookupIniWithDefault outDef cfg logSect outKey
    level  = mapLevel  $ lookupIniWithDefault lvlDef cfg logSect lvlKey
    logSect = pack "logging"
    outKey = pack "output"
    outDef = "stdout"
    mapOutput "stdout" = StdOut
    lvlKey = pack "level"
    mapLevel "info" = Info
    lvlDef = "info"

createTimestamp :: MonadIO m => Ini -> m String
createTimestamp cfg = timePrint tsfmt <$> liftIO dateCurrent
  where
    tsfmt = unpack $ lookupIniWithDefault tsfmtDef cfg cfgSect tsfmtKey
    cfgSect = pack "btrbkp"
    tsfmtKey = pack "timestampfmt"
    tsfmtDef = pack "YYYY-MM-DD_H-MI-S"

createSeparator :: Ini -> String
createSeparator cfg = unpack (lookupIniWithDefault (pack ":") cfg mainSect (pack "separator"))

createBackupRoot :: Ini -> FilePath
createBackupRoot = queryMainConfig "destination"

queryMainConfig :: Text -> Ini -> String
queryMainConfig key cfg = (either error unpack) result
  where
    result = lookupValue mainSect key cfg

mainSect :: Text
mainSect = pack "btrbkp"


lookupIniWithDefault :: Text -> Ini -> Text -> Text -> Text
lookupIniWithDefault def ini sect key = either (const def) id $ lookupValue sect key ini
