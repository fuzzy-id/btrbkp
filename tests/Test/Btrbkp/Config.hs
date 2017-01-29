{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Btrbkp.Config (tests) where

import Data.Ini (parseIni)
import qualified Data.Text as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Btrbkp.Config

tests :: TestTree
tests = $(testGroupGenerator)

case_read_empty_destinations_and_sources = res @?= expected
  where
    expected = Right (BtrbkpConfig [] [])
    res = configFromIni =<< (parseIni . T.unlines) [ "[btrbkp]"
                                                   , "destinations:"
                                                   , "sources:"
                                                   ]

case_read_with_destinations = res @?= expected
  where
    expected = Right (BtrbkpConfig [BtrbkpDestination "bar"] [])
    res = configFromIni =<< (parseIni . T.unlines) [ "[btrbkp]"
                                                   , "destinations: bar"
                                                   , "sources:"
                                                   ]

case_read_with_sources = res @?= expected
  where
    expected = Right (BtrbkpConfig [] [BtrbkpSource "baz"])
    res = configFromIni =<< (parseIni . T.unlines) [ "[btrbkp]"
                                                   , "destinations:"
                                                   , "sources: baz"
                                                   ]

case_read_with_destination_section = res @?= expected
  where
    expected = Right (BtrbkpConfig [BtrbkpDestination "baz"] [])
    res = configFromIni =<< (parseIni . T.unlines) [ "[btrbkp]"
                                                   , "destinations: foo"
                                                   , "sources:"
                                                   , "[foo]"
                                                   , "path=baz"
                                                   ]

