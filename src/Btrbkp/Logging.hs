module Btrbkp.Logging ( Level(..)
                      , bkpLog
                      , defSettings
                      , msg
                      , msgTo
                      , new
                      , setLogLevel
                      , setOutput
                      ) where

import Prelude hiding (log)

import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Logger (Level(..), Msg(), defSettings, log, msg, new, setLogLevel, setOutput)

import Btrbkp.Types

msgTo :: String -> String -> Msg -> Msg
msgTo s d = msg (s ++ " -> " ++ d)

bkpLog :: MonadIO m => Level -> (Msg -> Msg) -> Btrbkp m
bkpLog lvl msg = do l <- view logger
                    liftIO (log l lvl msg)
