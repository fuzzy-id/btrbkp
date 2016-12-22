{-# LANGUAGE TemplateHaskell #-}
module Libraries.Hourglass (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.TH (testGroupGenerator)

import Common.Helpers

import Data.Hourglass
import Time.System

tests :: TestTree
tests = $(testGroupGenerator)

case_valid_timeformat = res @?= "2016-12-21-12-53-30"
  where
    res = timePrint btrbkpTimeformat d
    d = DateTime (Date 2016 December 21) (TimeOfDay 12 53 30 0)

case_printed_format_is_padded = res @?= "2017-01-01-00-00-00"
  where
    res = timePrint btrbkpTimeformat d
    d = DateTime (Date 2017 January 01) (TimeOfDay 0 0 0 0)

btrbkpTimeformat = [ Format_Year4, dash, Format_Month2, dash, Format_Day2, dash
                   , Format_Hour,  dash, Format_Minute, dash, Format_Second]
  where
    dash = Format_Text '-'
