{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections
, TypeApplications
, ScopedTypeVariables #-}

module Montevideo.Monome.Test.EdoMath where

import Test.HUnit

import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Initial


tests :: Test
tests = TestList [
    TestLabel "test_et31ToLowXY" test_et31ToLowXY
  , TestLabel "test_pcToXys" test_pcToXys
  ]

test_pcToXys :: Test
test_pcToXys = TestCase $ do
  let ec = EdoConfig { _spacing = 6
                     , _edo = 31
                     , _skip = 1
                     }
  assertBool "" $ pcToXys ec (0,0) 0
    == [(0,0),(5,1),(4,7),(3,13),(10,2),(9,8),(8,14),(15,3),(14,9),(13,15)]
  assertBool "" $ pcToXys ec (0,1) 0
    == [(0,1),(5,2),(4,8),(3,14),(10,3),(9,9),(8,15),(15,4),(14,10)]
  assertBool "" $ pcToXys ec (0,0) 0 ==
                  pcToXys ec (0,0) 31
  assertBool "" $ pcToXys ec (1,2) 31 ==
                  pcToXys ec (1,2) 62

test_et31ToLowXY :: Test
test_et31ToLowXY = TestCase $ do
  let ec = EdoConfig { _spacing = 6
                     , _edo = 31 }

  assertBool "et31ToLowXY" $ et31ToLowXY ec       0  == (0,0)
  assertBool "et31ToLowXY" $ et31ToLowXY ec (31 + 0) == (0,0)

  assertBool "et31ToLowXY" $ et31ToLowXY ec       1  == (0,1)
  assertBool "et31ToLowXY" $ et31ToLowXY ec (31 + 1) == (0,1)

  assertBool "et31ToLowXY" $ et31ToLowXY ec       6  == (1,0)
  assertBool "et31ToLowXY" $ et31ToLowXY ec (31 + 6) == (1,0)

  assertBool "et31ToLowXY" $ et31ToLowXY ec       7  == (1,1)
  assertBool "et31ToLowXY" $ et31ToLowXY ec (31 + 7) == (1,1)
