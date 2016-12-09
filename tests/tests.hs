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
import qualified Libraries.TinyLog as LT

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where ingredients = testVolIngredient:defaultIngredients
        testVolIngredient = includingOptions [testVolDescr]

tests :: TestTree
tests = testGroup "Collected" [LB.tests, LT.tests]
