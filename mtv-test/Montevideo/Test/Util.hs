{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.Test.Util where

import Test.HUnit

import Montevideo.Util


tests :: Test
tests = TestList
  [ TestLabel "test_interleaves" test_interleaves
  , TestLabel "test_lcmRatios" test_lcmRatios
  , TestLabel "test_lines'" test_lines'
  ]

test_lines' :: Test
test_lines' = TestCase $ do
  assertBool ""      $ lines' '/' ""      == []
  assertBool ""      $ lines' '/' "abc"   == ["abc"]
  assertBool "/abc"  $ lines' '/' "/abc"  == ["","abc"]
  assertBool "/abc/" $ lines' '/' "/abc/" == ["","abc",""]
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
