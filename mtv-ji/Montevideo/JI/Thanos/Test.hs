module Montevideo.JI.Thanos.Test where

import           Data.Ratio

import Test.HUnit
import Montevideo.JI.Thanos.Thanos


tests :: Test
tests = TestList [
    TestLabel "test_best" test_best
  , TestLabel "test_primes" test_primes
  , TestLabel "test_primeIntervals" test_primeIntervals
  , TestLabel "test_feasibleSpacing" test_feasibleSpacing
  , TestLabel "test_shortWaysToReach" test_shortWaysToReach
  ]

test_shortWaysToReach :: Test
test_shortWaysToReach = TestCase $ do
  assertBool "Only one good choice." $
    shortWaysToReach 2 13 26 == [(2,0)]
  assertBool "Two equally good choices." $
    shortWaysToReach 1 6 9 == [(1,3),(2,-3)]
  assertBool "You could play this as easily on the 0th string but that's excluded from consideration." $
    shortWaysToReach 1 6 3 == [(1,-3)]
  assertBool "Two nearly equally good choices." $
    shortWaysToReach 2 13 36 == [(2,5),(4,-8)]

test_feasibleSpacing :: Test
test_feasibleSpacing = TestCase $ do
  assertBool "" $       feasibleSpacing 2 3
  assertBool "" $ not $ feasibleSpacing 2 4
  assertBool "" $       feasibleSpacing 6 7
  assertBool "" $ not $ feasibleSpacing 6 9

test_primeIntervals :: Test
test_primeIntervals = TestCase $ do
  assertBool "" $ primeIntervals 31 ==
    [ (10,5 % 4)
    , (14,11 % 8)
    , (18,3 % 2)
    , (22,13 % 8)
    , (25,7 % 4)
    , (31,2 % 1) ]

test_primes :: Test
test_primes = TestCase $ do
  assertBool "" $ elem 2 $ map fst primes

test_best :: Test
test_best = TestCase $ do
  assertBool "" $ best 12 (3/2) == 7
  assertBool "" $ best 12 (5/4) == 4
  assertBool "" $ best 31 (3/2) == 18
