{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Montevideo.Monome.Test.EdoMath where

import qualified Data.Map as M
import           Test.HUnit

import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types


tests :: Test
tests = TestList [
    TestLabel "test_pcToLowXY" test_pcToLowXY
  , TestLabel "test_pcToXys" test_pcToXys
  , TestLabel "test_xyToMonome" test_xyToMonome
  ]

test_xyToMonome :: Test
test_xyToMonome = TestCase $ do
  let app = EdoApp { _edoKeyboards = M.fromList
                     [ (Monome_256, Keyboard { _kbdShift = (0,1) } )
                     , (Monome_old, Keyboard { _kbdShift = (1,0) } ) ] }
  assertBool "" $ xyToMonome app Monome_256 Monome_old
    (0,1) == Right (1,0)
  assertBool "" $ xyToMonome app Monome_256 Monome_old
    (1,1) == Right (2,0)

test_pcToXys :: Test
test_pcToXys = TestCase $ do
  let ec = EdoConfig { _spacing = 6
                     , _edo = 31
                     , _skip = 1
                     , _gridVectors = Nothing
                     }
  assertBool "" $ pcToXys ec (0,0) 0
    == [(0,0),(5,1),(4,7),(3,13),(10,2),(9,8),(8,14),(15,3),(14,9),(13,15)]
  assertBool "" $ pcToXys ec (0,1) 0
    == [(0,1),(5,2),(4,8),(3,14),(10,3),(9,9),(8,15),(15,4),(14,10)]
  assertBool "" $ pcToXys ec (0,0) 0 ==
                  pcToXys ec (0,0) 31
  assertBool "" $ pcToXys ec (1,2) 31 ==
                  pcToXys ec (1,2) 62

test_pcToLowXY :: Test
test_pcToLowXY = TestCase $ do
  let ec = EdoConfig { _spacing = 6
                     , _skip = 1
                     , _edo = 31
                     , _gridVectors = Nothing }

  assertBool "pcToLowXY" $ pcToLowXY ec       0  == (0,0)
  assertBool "pcToLowXY" $ pcToLowXY ec (31 + 0) == (0,0)

  assertBool "pcToLowXY" $ pcToLowXY ec       1  == (0,1)
  assertBool "pcToLowXY" $ pcToLowXY ec (31 + 1) == (0,1)

  assertBool "pcToLowXY" $ pcToLowXY ec       6  == (0,6)
  assertBool "pcToLowXY" $ pcToLowXY ec (31 + 6) == (0,6)

  assertBool "pcToLowXY" $ pcToLowXY ec       7  == (0,7)
  assertBool "pcToLowXY" $ pcToLowXY ec (31 + 7) == (0,7)
