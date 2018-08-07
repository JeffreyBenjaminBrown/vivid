module Vivid.Jbb.Distrib.Test (tests) where

import           Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Museq
import Test.HUnit


tests = runTestTT $ TestList
  [ TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "findNextEvents" testFindNextEvents
  , TestLabel "testAllWaiting" testAllWaiting
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
  assertBool "valid, empty"                   $ museqIsValid
    $ Museq { _dur = 3, _vec = V.empty }
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ Museq { _dur = 0, _vec = V.empty }
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
