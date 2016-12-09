module Common.Helpers
       ( assertDirectoryExists
       , assertDirectoryExistsNot
       , destroySubvolRo
       , withTmpVol
       ) where

import Control.Exception (catchJust)
import Control.Monad (guard)
import System.IO.Error (isPermissionError)
import System.Process (callCommand)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Linux.Btrfs (createSubvol, destroySubvol)
import System.Random (newStdGen, randomRs)
import Test.Tasty (askOption)
import Test.Tasty.HUnit (Assertion, (@?), testCaseSteps)

import Common.TestOpts (testVol)


assertDirectoryExistsNot :: FilePath -> Assertion
assertDirectoryExistsNot d = 
  not <$> doesDirectoryExist d @? "Directory exists: " ++ d

assertDirectoryExists :: FilePath -> Assertion
assertDirectoryExists d =
   doesDirectoryExist d @? "Directory does not exist: " ++ d

destroySubvolRo :: FilePath -> IO ()
destroySubvolRo vol = catchJust
                        (guard . isPermissionError)
                        (destroySubvol vol)
                        (\_ -> do callCommand mkVolRW
                                  destroySubvol vol)
  where mkVolRW = "btrfs property set -t subvol " ++ vol ++ " ro false"

withTmpVol testFun title = askOption $ \vol -> testCaseSteps title $ \step ->
  if testVol vol == ""
    then step "Skipping (no test-vol passed)"
    else do step "Probing test environment"
            (assertDirectoryExists . testVol) vol
            postfix <- take 6 . randomRs ('a', 'z') <$> newStdGen

            let rootVol = testVol vol </> "test_" ++ postfix
            step $ "Test volume is: " ++ rootVol
            assertDirectoryExistsNot rootVol
            step $ "Creating subvolume: " ++ rootVol
            createSubvol rootVol
            assertDirectoryExists rootVol

            testFun rootVol step
            
            step $ "Destroying subvolume: " ++ rootVol
            destroySubvol rootVol
            assertDirectoryExistsNot rootVol
