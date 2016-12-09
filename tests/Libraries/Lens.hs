{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Libraries.Lens (tests) where

import Prelude hiding (log)

import System.Logger (Logger(), defSettings, log, Level(Debug), msg, setFormat)
import System.Logger.Message (ToBytes())
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Common.Helpers

import Control.Lens hiding (Level)
import Control.Monad.Reader

data BackupEnv = BackupEnv { _logger :: Logger }

makeLenses ''BackupEnv

tests :: TestTree
tests = $(testGroupGenerator)

case_view_logger = 
    contentOfLoggerOnTempFile s (runReaderT f . BackupEnv) >>= (@=? "D, foo\n")
  where 
    f = backupLog Debug "foo"
    s = setFormat Nothing defSettings


backupLog  :: (MonadReader BackupEnv m, ToBytes a, MonadIO m)
              => Level -> a -> m ()
backupLog level m = view logger >>= \l -> log l level (msg m)
