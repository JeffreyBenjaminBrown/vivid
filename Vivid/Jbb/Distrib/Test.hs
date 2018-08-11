module Vivid.Jbb.Distrib.Test (tests) where

import           Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Museq
import Vivid.Jbb.Distrib.Transform
import Vivid.Jbb.Distrib.Types
import Test.HUnit


tests = runTestTT $ TestList
  [ TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "findNextEvents" testFindNextEvents
  , TestLabel "testAllWaiting" testAllWaiting
  , TestLabel "testAppend" testAppend
  , TestLabel "testStackAsIfEqualLength" testStackAsIfEqualLength
  , TestLabel "testStack" testStack
  , TestLabel "testRev" testRev
  , TestLabel "testEarlyAndLate" testEarlyAndLate
  , TestLabel "testFastAndSlow" testFastAndSlow
  , TestLabel "testDenseAndSparse" testDenseAndSparse
  ]

testPrevPhase0 = TestCase $ do
  assertBool "" $ prevPhase0 0 10 11 == 10
  assertBool "" $ prevPhase0 0 10 10 == 10
  assertBool "" $ prevPhase0 0 20 41 == 40
  assertBool "" $ prevPhase0 0 20 59 == 40

testNextPhase0 = TestCase $ do
  assertBool "" $ nextPhase0 0 10 11 == 20
  assertBool "" $ nextPhase0 0 10 10 == 10
  assertBool "" $ nextPhase0 0 20 41 == 60
  assertBool "" $ nextPhase0 0 20 59 == 60

testMuseqIsValid = TestCase $ do
  let vempty = V.empty :: V.Vector (RTime, ())
  assertBool "valid, empty"                   $ museqIsValid
    $ (museq 3 [] :: Museq ())
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ (museq 0 [] :: Museq ())
  assertBool "valid, nonempty"                $ museqIsValid
    $ (museq 1 [(0, New Boop "marge")])
  assertBool "invalid, time > 1" $ not $ museqIsValid
    $ (museq 1 [(1.5, New Boop "marge")])

testFindNextEvents = TestCase $ do
  let bp f = New Boop $ show f
      events = [(0,bp 1),(0.25,bp 2),(0.5,bp 3),(0.5,bp 4),(0.75,bp 5)]
  assertBool "testFindNextEvents, midway" $
    (findNextEvents 1 10 29 $ museq 2 events)
      -- findNextEvents time0 globalPeriod now museq
    == (2, Prelude.map snd $ V.toList $ V.slice 2 2 $ V.fromList events)
      -- last phase 0 was at 21s, so now (29) is 2s before halfway through
  assertBool "testFindNextEvents, end (wraparound)" $
    (findNextEvents 1 10 39.5 $ museq 2 events)
    == (1.5, Prelude.map snd $ V.toList $ V.slice 0 1 $ V.fromList events)

  let events' = tail events -- PITFALL, maybe confusing: below I use both
  assertBool "testFindNextEvents, no zero event, this cycle" $
    (findNextEvents 1 50 123 $ museq 2 events') -- next is 126
    == (3, Prelude.map snd $ V.toList $ V.slice 1 1 $ V.fromList events)
  assertBool "testFindNextEvents, no zero event, next cycle" $
    (findNextEvents 1 50 100 $ museq 2 events') -- next is 126
    == (26, Prelude.map snd $ V.toList $ V.slice 1 1 $ V.fromList events)

testAllWaiting = TestCase $ do
  now <- unTimestamp <$> getTime
  dist <- newDistrib
  let mm = mTimeMuseqs dist
  swapMVar mm $ M.fromList [("a",(now+1,emptyMuseq))
                           ,("b",(now+1,emptyMuseq))]
  assertBool "all waiting" =<< allWaiting dist
  swapMVar mm $ M.fromList [("a",(now-1,emptyMuseq))
                           ,("b",(now+1,emptyMuseq))]
  assertBool "one is not waiting" =<< not <$> allWaiting dist

testAppend = TestCase $ do
  let m = museq 1 [(1/2,())]
      n = museq 2 [(1/2,())]
  assertBool "length 1 midpoint ++ length 2 midpoint" $
    append m n == museq 3 [(1/6,())
                          ,(4/6,())
                          ]

testStackAsIfEqualLength = TestCase $ do
  let a = museq 2 [(1/2,"a")]
      z = museq (-3/0) [(2/3,"z")]
  assertBool "stackAsIfEqualLength" $ stackAsIfEqualLength a z ==
    museq 2 [(1/2,"a"),
              (2/3,"z")]

testStack = TestCase $ do
  let a = museq 2 [(0,"a")]
      z = museq 3 [(1/2,"z")]
  assertBool "" $ stack a z == museq 6 [(0,"a")
                                       ,(1%4,"z")
                                       ,(1%3,"a")
                                       ,(2%3,"a")
                                       ,(3%4,"z") ]

testRev = TestCase $ do
  let a = museq 2 [(0,"a")
                  ,(1/3,"b")
                  ,(1/2,"c")]
  assertBool "rev" $ rev a == museq 2 [(0,"a")
                                      ,(1/2,"c")
                                      ,(2/3,"b")]

testEarlyAndLate = TestCase $ do
  let a = museq 10 [(0,"a"),(1%2,"b")]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]
  assertBool "early" $ _vec (early 11 a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]
  assertBool "early" $ _vec (early (-9) a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]

  assertBool "late" $ _vec (late 19 a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]
  assertBool "late" $ _vec (late 9 a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]
  assertBool "late" $ _vec (late (-1) a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]
  assertBool "late" $ _vec (late (-11) a) ==
    V.fromList [(4%10,"b"),(9%10,"a")]

testFastAndSlow = TestCase $ do
  let a = museq 10 [(0,"a"),(1%2,"b")]
  assertBool "fast" $ (fast 2 a) == a {_dur = 5}
  assertBool "slow" $ (slow 2 a) == a {_dur = 20}

testDenseAndSparse = TestCase $ do
  let x = museq 10 [(0 % 1,"a"),(1 % 2,"b")]
  return ()
  assertBool "dense 2" $ dense 2 x == museq 10 [(0 % 1,"a")
                                               ,(1 % 4,"b")
                                               ,(1 % 2,"a")
                                               ,(3 % 4,"b")]
  assertBool "dense 1.5" $ dense 1.5 x == museq 10 [(0 % 1,"a")
                                                   ,(1 % 3,"b")
                                                   ,(2 % 3,"a")]
