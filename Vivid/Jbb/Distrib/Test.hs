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
    $ Museq { _dur = 3, _vec = vempty }
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ Museq { _dur = 0, _vec = vempty }
  assertBool "valid, nonempty"                $ museqIsValid
    $ Museq { _dur = 1, _vec = V.fromList [(0, New Boop "marge")]}
  assertBool "invalid, time > 1" $ not $ museqIsValid
    $ Museq { _dur = 1, _vec = V.fromList [(1.5, New Boop "marge")]}

testFindNextEvents = TestCase $ do
  let bp f = New Boop $ show f
      events = [(0,bp 1),(0.25,bp 2),(0.5,bp 3),(0.5,bp 4),(0.75,bp 5)]
  assertBool "testFindNextEvents, midway" $
    (findNextEvents 1 10 29 $ Museq 2 $ V.fromList events)
      -- findNextEvents time0 globalPeriod now museq
    == (2, Prelude.map snd $ V.toList $ V.slice 2 2 $ V.fromList events)
      -- last phase 0 was at 21s, so now (29) is 2s before halfway through
  assertBool "testFindNextEvents, end (wraparound)" $
    (findNextEvents 1 10 39.5 $ Museq 2 $ V.fromList events)
    == (1.5, Prelude.map snd $ V.toList $ V.slice 0 1 $ V.fromList events)

  let events' = tail events -- PITFALL, maybe confusing: below I use both
  assertBool "testFindNextEvents, no zero event, this cycle" $
    (findNextEvents 1 50 123 $ Museq 2 $ V.fromList events') -- next is 126
    == (3, Prelude.map snd $ V.toList $ V.slice 1 1 $ V.fromList events)
  assertBool "testFindNextEvents, no zero event, next cycle" $
    (findNextEvents 1 50 100 $ Museq 2 $ V.fromList events') -- next is 126
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
  let m = Museq { _dur = 1, _vec = V.fromList [(1/2,())] }
      n = Museq { _dur = 2, _vec = V.fromList [(1/2,())] }
  assertBool "length 1 midpoint ++ length 2 midpoint" $
    append m n == Museq { _dur = 3, _vec = V.fromList [(1/6,())
                                                      ,(4/6,())
                                                      ] }

testStackAsIfEqualLength = TestCase $ do
  let a = Museq { _dur = 2,      _vec = V.fromList [(1/2,"a")] }
      z = Museq { _dur = (-3/0), _vec = V.fromList [(2/3,"z")] }
  assertBool "stackAsIfEqualLength" $ stackAsIfEqualLength a z ==
    Museq { _dur = 2, _vec = V.fromList [(1/2,"a"),
                                         (2/3,"z")] }

testStack = TestCase $ do
  let a = Museq { _dur = 2, _vec = V.fromList [(0,"a")] }
      z = Museq { _dur = 3, _vec = V.fromList [(1/2,"z")] }
  assertBool "" $ stack a z ==
    Museq { _dur = 6
          , _vec = V.fromList [(0,"a")
                              ,(1%4,"z")
                              ,(1%3,"a")
                              ,(2%3,"a")
                              ,(3%4,"z")
                              ] }
