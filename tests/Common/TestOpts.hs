module Common.TestOpts (testVol, testVolDescr) where

import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Test.Tasty.Options (IsOption(..), OptionDescription(..))

newtype TestVol = TestVol { testVol :: FilePath }
                          deriving (Show)

instance IsOption TestVol where
  defaultValue = TestVol ""
  parseValue = Just . TestVol
  optionName = Tagged "test-vol"
  optionHelp = Tagged "Path to subvolume on which tests are to be carried out."

testVolDescr :: OptionDescription
testVolDescr = Option (Proxy :: Proxy TestVol)
