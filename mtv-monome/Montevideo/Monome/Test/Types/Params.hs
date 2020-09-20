module Montevideo.Monome.Test.Types.Params where

import Data.Either
import Test.HUnit

import Montevideo.Monome.Types.Params
import Montevideo.Synth


tests :: Test
tests = TestList
  [ TestLabel "test_paramGroup_toParam" test_paramGroup_toParam
  , TestLabel "test_paramGroup_toXy" test_paramGroup_toXy
  ] 

test_paramGroup_toXy :: Test
test_paramGroup_toXy = TestCase $ do
  assertBool "" $ paramGroup_toXy PG_end == (2,2)

test_paramGroup_toParam :: Test
test_paramGroup_toParam = TestCase $ do
  assertBool "0" $          paramGroup_toParam PG_source 0 == Right Zot_amp
  assertBool "2" $ isLeft $ paramGroup_toParam PG_source 5
