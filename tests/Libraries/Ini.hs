{-# LANGUAGE TemplateHaskell #-}
module Libraries.Ini (tests) where

import Control.Exception (try)
import Data.HashMap.Strict (fromList)
import GHC.IO.Exception (IOErrorType(NoSuchThing), ioe_type)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@=?), (@?=), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Data.Ini (Ini(), readIniFile, sections, unIni)

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

withTmpDir = withSystemTempDirectory "btrbk-tests"
