module Vivid.Jbb.Distrib.Test (tests) where

import           Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Museq
import Test.HUnit


tests = runTestTT $ TestList
  [ TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "findNextEvents" testFindNextEvents
  , TestLabel "testAllWaiting" testAllWaiting
  ]

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
  let bp s = New Boop s
      events = [(0,bp "a"),(0.25,bp"b"),(0.5,bp"c"),(0.5,bp"d"),(0.75,bp"e")]
  assertBool "testFindNextEvents" $
    (findNextEvents 1 10 29 $ Museq 2 $ V.fromList events)
      -- findNextEvents time0 globalPeriod now museq
    == (2, Prelude.map snd $ V.toList $ V.slice 2 2 $ V.fromList events)
      -- last phase 0 was at 21s, so now (29) is 2s before halfway through

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
