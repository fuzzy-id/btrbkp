{-# LANGUAGE TemplateHaskell #-}
module Libraries.Btrfs (tests) where

import System.Linux.Btrfs (snapshot)
import Test.Tasty (TestTree)

import Common.Helpers

tests :: TestTree
tests = withTmpVol test_snapshot_creation "Snapshot Creation"

test_snapshot_creation rootVol step =
  do step $ "Creating a snapshot: " ++ snapVol
     assertDirectoryExistsNot snapVol
     snapshot rootVol snapVol True
     assertDirectoryExists snapVol

     step $ "Destroying snapshot: " ++ snapVol
     destroySubvolRo snapVol
     assertDirectoryExistsNot snapVol
  where snapVol = rootVol ++ "_snap"
