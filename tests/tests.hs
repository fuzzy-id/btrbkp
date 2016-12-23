{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Tasty ( TestTree
                  , defaultIngredients
                  , defaultMainWithIngredients
                  , includingOptions
                  , testGroup
                  )


import Common.TestOpts (testVolDescr)
import qualified Libraries.Btrfs as LB
import qualified Libraries.Hourglass as LH
import qualified Libraries.Ini as LI
import qualified Libraries.Lens as LL
import qualified Libraries.TinyLog as LT
import qualified Test.Btrbkp as TB
import qualified Test.Btrbkp.Config as TBC

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where ingredients = testVolIngredient:defaultIngredients
        testVolIngredient = includingOptions [testVolDescr]

tests :: TestTree
tests = testGroup "Collected" [LB.tests, LH.tests, LI.tests, LL.tests, LT.tests, TB.tests, TBC.tests]
