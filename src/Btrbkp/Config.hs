{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Btrbkp.Config where

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
                          <*> createDestinationBuilder cfg
                          <*> pure (getDestination cfg)

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

createDestinationBuilder :: MonadIO m => Ini -> m (Text -> FilePath -> FilePath)
createDestinationBuilder cfg =
    do timestamp <- timePrint tsfmt <$> liftIO dateCurrent
       let dest = getDestination cfg
       return (\n s -> dest </> timestamp ++ sep ++ unpack n ++ sep ++ normalize s)
  where
    tsfmt = unpack $ lookupIniWithDefault tsfmtDef cfg cfgSect tsfmtKey
    cfgSect = pack "btrbkp"
    tsfmtKey = pack "timestampfmt"
    tsfmtDef = pack "YYYY-MM-DD_H-MI-S"
    sep = ":"

getDestination cfg = (either error unpack) destination
  where
    destination = lookupValue cfgSect "destination" cfg
    cfgSect = pack "btrbkp"

normalize d = (foldr1 (\x y -> x ++ '-':y) . filter ((/= 0) . length) . map (filter (not . isPathSeparator)) . splitDirectories) d


lookupIniWithDefault :: Text -> Ini -> Text -> Text -> Text
lookupIniWithDefault def ini sect key = either (const def) id $ lookupValue sect key ini
