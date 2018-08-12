module Vivid.Jbb.Distrib.Test where

import Control.Concurrent.MVar
import qualified Control.Lens as L
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.ActNow
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Join
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
  , TestLabel "testStack" testStack
  , TestLabel "testRev" testRev
  , TestLabel "testEarlyAndLate" testEarlyAndLate
  , TestLabel "testFastAndSlow" testFastAndSlow
  , TestLabel "testDenseAndSparse" testDenseAndSparse
  , TestLabel "testExplicitReps" testExplicitReps
  , TestLabel "testMuseqsDiff" testMuseqsDiff
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

testStack = TestCase $ do
  let y = museq 2 [(0,"y")]
      z = museq 3 [(1,"z")]
  assertBool "stack" $ stack y z ==
    L.set dur (_dur y) ( museq 6 [(0,"y")
                                 ,(1,"z")
                                 ,(2,"y")
                                 ,(4,"y")
                                 ,(4,"z")] )
  assertBool "stack" $ stack (L.set dur 1 y) z ==
    L.set dur 1 ( museq 6 [(0,"y")
                          ,(1,"z")
                          ,(2,"y")
                          ,(4,"y")
                          ,(4,"z")] )
  assertBool "stack, where timeToRepeat differs from timeToPlayThrough"
    $ stack (L.set sup 1 y) z ==
    L.set dur 2 ( museq 3 [(0,"y")
                          ,(1,"y")
                          ,(1,"z")
                          ,(2,"y") ] )

testRev = TestCase $ do
  let a = museq 2 [(0,"a")
                  ,(1/3,"b")
                  ,(1/2,"c")]
  assertBool "rev" $ rev a == museq 2 [(0,"a")
                                      ,(3/2,"c")
                                      ,(5/3,"b")]

testEarlyAndLate = TestCase $ do
  let a = museq 10 [(0,"a"),(1,"b")]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [(0,"b"),(9,"a")]
  assertBool "early" $ _vec (early 11 a) ==
    V.fromList [(0,"b"),(9,"a")]
  assertBool "early" $ _vec (early (-9) a) ==
    V.fromList [(0,"b"),(9,"a")]

  assertBool "late" $ _vec (late 19 a) ==
    V.fromList [(0,"b"),(9,"a")]
  assertBool "late" $ _vec (late 9 a) ==
    V.fromList [(0,"b"),(9,"a")]
  assertBool "late" $ _vec (late (-1) a) ==
    V.fromList [(0,"b"),(9,"a")]
  assertBool "late" $ _vec (late (-11) a) ==
    V.fromList [(0,"b"),(9,"a")]

testFastAndSlow = TestCase $ do
  let a = museq 10 [(0,"a"),(2,"b")]
  assertBool "fast" $ (fast 2 a) == museq 5 [(0,"a"),(1,"b")]
  assertBool "slow" $ (slow 2 a) == museq 20 [(0,"a"),(4,"b")]

testDenseAndSparse = TestCase $ do
  let x = museq 10 [(0 % 1,"a"),(2,"b")]
  assertBool "dense" $ dense 2 x ==
    L.set dur 10 (museq 5 [(0 % 1,"a"),(1,"b")])
  assertBool "sparse" $ sparse 2 x ==
    L.set dur 10 (museq 20 [(0 % 1,"a"),(4,"b")])

testExplicitReps = TestCase $ do
  let y = Museq {_dur = 3, _sup = 4, _vec = V.fromList [(0,()),(1,())]}
  assertBool "explicitReps" $ explicitReps y == [ V.fromList [(0,()), (1,())]
                                                , V.fromList [(4,()), (5,())]
                                                , V.fromList [(8,())]
                                                , V.fromList [(9,())]
                                                ]
  assertBool "unsafeExplicitReps" $ unsafeExplicitReps 24 y ==
     [ V.fromList [(0,()), (1,())]
     , V.fromList [(4,()), (5,())]
     , V.fromList [(8,())]
     , V.fromList [(9,())]
     , V.fromList [(12,()), (13,())]
     , V.fromList [(16,()), (17,())]
     , V.fromList [(20,())]
     , V.fromList [(21,())]
     ]

testAppend = TestCase $ do
    let a = museq 1 [(0,"a")]
        a2 = a {_sup = 2}
        a12 = a {_sup = 1%2}
        a32 = a {_sup = 3%2}
        b = museq 1 [(0,"b")]
    assertBool "testAppend" $ append a b == museq 2 [(0,"a"),(1,"b")]
    assertBool "testAppend" $ append a2 b ==
      let m = museq 2 [(0,"a"),(1,"b"),(3,"b")] in m {_sup = 4}
    assertBool "testAppend" $ append a12 b ==
      museq 2 [(0,"a"),(1%2,"a"),(1,"b")]
    assertBool "testAppend" $ append a32 b ==
      let m = museq 2 [(0,"a"), (1,"b"), (2+1/2,"a"), (3,"b"), (5,"b")]
      in m {_sup = 6}

-- | I'm not really sure what rotate should do.
testRep = TestCase $ do
  let a = museq 6 [(0,"a")]
  assertBool "rep int" $ rep 2 a ==
    L.set dur 12 (museq 6 [(0,"a")])
  assertBool "rep fraction" $ rep (3/2) a ==
    L.set dur 9 (museq 6 [(0,"a")])

  let b = museq 6 [(0,"a"),(3,"b")]
  assertBool "rotate int" $ rotate 2 b ==
    L.set dur 6 (museq 12 [(0,"a"),(6,"b")])

testMuseqsDiff = TestCase $ do
  let msg = ("amp",1)
      m1 = M.fromList [("a", museq 10 [(0, Send Boop "1" msg )])
                      ,("b", museq 15 [(0, Send Boop "1" msg)
                                      ,(10,Send Boop "2" msg)
                                      ] ) ]
      m2 = M.fromList [("a", museq 10 [(0, Send Vap "2" msg)])
                      ,("b", museq 15 [(0, Send Boop "2" msg)
                                      ,(10,Send Boop "3" msg)
                                      ] ) ]
  assertBool "museqDiff" $ museqsDiff m1 m2 == ( [ (Boop,"1") ]
                                               , [ (Vap ,"2")
                                                 , (Boop,"3")
                                                 ] )
  assertBool "museqDiff" $ museqsDiff m2 m1 == ( [ (Vap ,"2")
                                                 , (Boop,"3") ]
                                               , [ (Boop,"1") ]
                                               )
