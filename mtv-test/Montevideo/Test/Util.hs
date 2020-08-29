{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.Test.Util where

import Test.HUnit

import Montevideo.Util


tests :: Test
tests = TestList
  [ TestLabel "test_interleaves" test_interleaves
  , TestLabel "test_lcmRatios" test_lcmRatios
  , TestLabel "test_lines'" test_lines'
  , TestLabel "test_logScale" test_logScale
  , TestLabel "test_linScale" test_linScale
  ]

test_linScale :: Test
test_linScale = TestCase $ do
  assertBool "" $
    linScale (100,110) (200,250) 102
    == 210
  assertBool "" $
    linScale (-50,0) (-300,-200) (-10)
    == (-220)

test_logScale :: Test
test_logScale = TestCase $ do
  let near x y = x/y > 0.9 && x/y < 1.1
  assertBool "logarithmically halfway from 1 to 100 is 10" $
    near (logScale (0 ,10) (1,100)    5) 10
  assertBool "logarithmically halfway from 1 to 100 is 10" $
    near (logScale (10,20) (1,100)   15) 10
  assertBool "logarithmically halfway from 10 to 1000 is 100" $
    near (logScale (0 ,10) (10,1000)  5) 100
  assertBool "logarithmically 5/8 of the way from 1 to 256 is 2**5" $
    near (logScale  (10,18) (1,256)  15) 32

test_lines' :: Test
test_lines' = TestCase $ do
  assertBool ""      $ lines' '/' ""      == []
  assertBool ""      $ lines' '/' "abc"   == ["abc"]
  assertBool "/abc -- note that a leading '/' is ignored"
                     $ lines' '/' "/abc"  == ["abc"]
  assertBool "/abc/" $ lines' '/' "/abc/" == ["abc",""]
  assertBool "abc/"  $ lines' '/' "abc/"  == ["abc",""]

test_lcmRatios :: Test
test_lcmRatios = TestCase $ do
  assertBool "lcmRatios 2 3 == 6"            $ lcmRatios 2 3 == 6
  assertBool "lcmRatios (2/3) (3/2) == 6"    $ lcmRatios (2/3) (3/2) == 6
  assertBool "lcmRatios (1/2) (3/4) == 3/2"  $ lcmRatios (1/2) (3/4) == 3/2
  assertBool "lcmRatios (5/3) (4/3) == 20/3" $ lcmRatios (5/3) (4/3) == 20/3

test_interleaves :: Test
test_interleaves = TestCase $ do
  assertBool "1" $ interleaves
    [[1,2,3],[11,12,13],[21,22,23,24]]
    == [1,11,21,2,12,22,3,13,23]
