{-# LANGUAGE TemplateHaskell #-}
module Libraries.TinyLog (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), (@?=), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Common.Helpers

import Prelude hiding (log)
import System.Logger 

tests :: TestTree
tests = $(testGroupGenerator)

case_log_to_file_and_read_back = 
    ((unlines $ zipWith (\l n -> l:", " ++ show n) "TDIWEF" [0..]) @=?) =<< contentOfLoggerOnTempFile s f
  where
    s = (setLogLevel Trace . setFormat Nothing) defSettings
    f logger = mapM_ (uncurry (log logger)) [ (Trace, msg "0")
                                            , (Debug, msg "1")
                                            , (Info,  msg "2")
                                            , (Warn,  msg "3")
                                            , (Error, msg "4")
                                            , (Fatal, msg "5")
                                            ]

case_level_Trace_not_logged_by_default = contentOfLoggerOnTempFile defSettings f >>= (@?= "")
  where
    f logger = log logger Trace (msg "foo")
