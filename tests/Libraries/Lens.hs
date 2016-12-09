{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Libraries.Lens (tests) where

import Prelude hiding (log)

import System.Logger (Logger(), defSettings, log, Level(Debug), msg, setFormat)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Common.Helpers

import Control.Lens
import Control.Monad.Reader

data BackupEnv = BackupEnv { _logger :: Logger }

makeLenses ''BackupEnv

tests :: TestTree
tests = $(testGroupGenerator)

case_view_logger = contentOfLoggerOnTempFile s (runReaderT f . BackupEnv) >>= (@=? "D, foo\n")
  where 
    f = view logger >>= \l -> log l Debug (msg "foo")
    s = setFormat Nothing defSettings
