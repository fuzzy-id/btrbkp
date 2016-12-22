{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Libraries.Ini (tests) where

import Control.Exception (try)
import Data.HashMap.Strict (fromList)
import qualified Data.Text as T
import GHC.IO.Exception (IOErrorType(NoSuchThing), ioe_type)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), (@?=), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Data.Ini
import Btrbkp.Config (listP)

tests :: TestTree
tests = $(testGroupGenerator)

case_parse_non_existent_file = withTmpDir f
  where f d = let fname = d </> "nonexistent"
              in do Left e <- try (readIniFile fname)
                    ioe_type e @?= NoSuchThing

case_parse_empty_file = withTmpDir f
  where
    f d = let fname = d </> "empty_file"
          in do writeFile fname ""
                Right res <- readIniFile fname
                sections res @?= []
                unIni res @?= fromList []

case_assignment_without_section_is_ignored = withTmpDir f
  where
    f d = let fname = d </> "some"
          in do writeFile fname "foo: bar\n"
                Right res <- readIniFile fname
                sections res @?= []
                unIni res @?= fromList []

case_looking_up_existent_value = res @?= expected
  where
    expected = Right "baz"
    res = lookupValue "foo" "bar" ini

case_lookup_empty_value = res @?= Right ""
  where
    res = lookupValue "foo" "blah" ini

case_lookupValue_misses_key = res @?= expected
  where
    expected = Left "Couldn't find key: borg"
    res = lookupValue "foo" "borg" ini

case_lookupValue_misses_section = res @?= expected
  where
    expected = Left "Couldn't find section: bar"
    res = lookupValue "bar" "borg" ini

case_parse_list_with_one_entry = res @?= expected
  where
    expected = Right ["baz"]
    res = parseValue "foo" "bar" listP ini

case_parse_list_with_two_entries = res @?= expected
  where
    expected = Right ["boom", "bang"]
    res = parseValue "foo" "buh" listP ini

case_parse_list_with_no_entries = res @?= Right []
  where
    res = parseValue "foo" "blah" listP ini

ini :: Ini
ini = (either error id . parseIni . T.unlines) ["[foo]", "bar: baz", "buh: boom,bang", "blah:"]

withTmpDir = withSystemTempDirectory "btrbk-tests"
