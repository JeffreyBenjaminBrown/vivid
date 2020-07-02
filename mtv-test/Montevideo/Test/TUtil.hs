{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.Test.TUtil where

import Test.HUnit

import Montevideo.Util


test_module_util :: Test
test_module_util = TestList [
    TestLabel "test_interleaves" test_interleaves
    ]

test_interleaves :: Test
test_interleaves = TestCase $ do
  assertBool "1" $ interleaves
    [[1,2,3],[11,12,13],[21,22,23,24]]
    == [1,11,21,2,12,22,3,13,23]
