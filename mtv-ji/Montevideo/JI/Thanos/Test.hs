{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.JI.Thanos.Test where

import           Data.Ratio

import Test.HUnit
import Montevideo.JI.Lib
import Montevideo.JI.Thanos.SearchParams
import Montevideo.JI.Thanos.Thanos
import Montevideo.JI.Thanos.Thanos2


tests :: Test
tests = TestList [
    TestLabel "test_best'" test_best'
  , TestLabel "test_primes" test_primes
  , TestLabel "test_primeIntervals" test_primeIntervals
  , TestLabel "test_relativelyPrime" test_relativelyPrime
  , TestLabel "test_shortWaysToReach" test_shortWaysToReach
  , TestLabel "test_choices" test_choices
  , TestLabel "test_bestError" test_bestError
  , TestLabel "test_guitarSpot" test_guitarSpot
  ]

test_guitarSpots :: Test
test_guitarSpots = TestCase $ do
  assertBool "" $ guitarSpots 31 6 1 (primesOctave1 13)
    == [ (0,0,0)
       , (31,5,1)
       , (18,3,0)
       , (10,1,4) -- One might expect 10,2,-2, but this is right -- it's finding not the closest, but rather the first with a positive fret value.
       , (25,4,1)
       , (5,0,5)
       , (14,2,2)
       , (22,3,4) ]

test_guitarSpot :: Test
test_guitarSpot = TestCase $ do
  assertBool "" $ guitarSpot 5 2 10 == (2,0)
  assertBool "" $ guitarSpot 7 2 1 == (-1,4)
  assertBool "" $ guitarSpot 5 3 1 == (-1,2)
  assertBool "" $ guitarSpot 5 3 2 == (-2,4)

test_bestError :: Test
test_bestError = TestCase $ do
  assertBool "" $ let b = abs $ bestError 12 (3/2)
    in b < 0.002 && b > 0.001
  assertBool "" $ let b = abs $ bestError 12 (5/4)
    in b < 0.02 && b > 0.01
  assertBool "" $ let r = 5/4 in
    abs (bestError 12 r) > abs (bestError 31 r)
  assertBool "" $ let r = 3/2 in
    abs (bestError 12 r) < abs (bestError 31 r)

test_choices :: Test
test_choices = TestCase $ do
  assertBool "" $
    choices [[1],[2,3],[4,5,6],[0],[0],[0]]
    == [ [1,2,4,0,0,0]
       , [1,2,5,0,0,0]
       , [1,2,6,0,0,0]
       , [1,3,4,0,0,0]
       , [1,3,5,0,0,0]
       , [1,3,6,0,0,0] ]
  assertBool "If any list is empty, the result is too" $
    choices [[],[2,3],[4,5,6],[0],[0],[0]]
    == []

-- PITFALL|TODO: This code breaks if I change the parameter
-- `isForGuitar` from True to False. Better would be
-- to take the whole set of parameters as a parameter to the function,
-- rather than hard-coding them. (But this code is no longer in use.)
test_shortWaysToReach :: Test
test_shortWaysToReach = TestCase $ do
  assertBool "Only one good choice." $
    shortWaysToReach 2 13 26 == [(2,0)]
  assertBool "SOme equally good choices." $
    shortWaysToReach 1 6 9 == [(0,9),(1,3),(2,-3),(3,-9)]
  assertBool "On a guitar, the 0-string solution is excluded." $
    shortWaysToReach 1 6 3 ==
    (if isForGuitar then filter ((/=) 0 . fst) else id)
    [(-1,9),(0,3),(1,-3),(2,-9)]
  assertBool "Two nearly equally good choices." $
    shortWaysToReach 2 13 36 == [(2,5),(4,-8)]

test_relativelyPrime :: Test
test_relativelyPrime = TestCase $ do
  assertBool "" $       relativelyPrime 1 3
  assertBool "" $       relativelyPrime 2 1
  assertBool "" $       relativelyPrime 2 3
  assertBool "" $ not $ relativelyPrime 2 4
  assertBool "" $       relativelyPrime 6 7
  assertBool "" $ not $ relativelyPrime 6 9

test_primeIntervals :: Test
test_primeIntervals = TestCase $ do
  assertBool "" $ take 7 (primeIntervals 31 100) ==
    [ ( 0, 1 % 1)
    , (31, 2 % 1)
    , (18, 3 % 2)
    , (10, 5 % 4)
    , (25, 7 % 4)
    , ( 5, 9 % 8)
    , (14,11 % 8) ]

test_primes :: Test
test_primes = TestCase $ do
  assertBool "" $ elem 2 $ map fst $ primesAnd1 31

test_best' :: Test
test_best' = TestCase $ do
  assertBool "" $ best' 12 (3/2) == 7
  assertBool "" $ best' 12 (5/4) == 4
  assertBool "" $ best' 31 (3/2) == 18
