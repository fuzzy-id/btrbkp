module Test.Btrbkp (tests) where

import qualified Data.Text as T
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Linux.Btrfs (createSubvol)
import Test.Tasty (TestTree(), testGroup)

import Common.Helpers

import Btrbkp
import Btrbkp.Config

tests :: TestTree
tests = testGroup "Btrbkp tests" [test_simple_backup]

test_simple_backup = withTmpVol f "Simple Backup"
  where f root _ = let src = root </> "source"
                       dest = root </> "dest"
                       cfg = BtrbkpConfig 
                               [(BtrbkpDestination . T.pack) dest]
                               [(BtrbkpSource . T.pack) src]
                   in do createDirectory dest
                         createSubvol src
                         runBkp cfg
