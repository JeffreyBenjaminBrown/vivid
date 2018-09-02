module Vivid.Jbb.Dispatch.Test.Auto where

import Control.Concurrent.MVar
import qualified Control.Lens as L
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Dispatch.Join
import Vivid.Jbb.Dispatch.Internal.Join
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Transform
import Vivid.Jbb.Dispatch.Types
import Test.HUnit


tests = runTestTT $ TestList
  [ TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "testAppend" testAppend
  , TestLabel "testRep" testRep
  , TestLabel "testStack" testStack
  , TestLabel "testRev" testRev
  , TestLabel "testEarlyAndLate" testEarlyAndLate
  , TestLabel "testFastAndSlow" testFastAndSlow
  , TestLabel "testDenseAndSparse" testDenseAndSparse
  , TestLabel "testExplicitReps" testExplicitReps
  , TestLabel "testMapMuseqsDiff" testMapMuseqsDiff
  , TestLabel "testArc" testArc
  , TestLabel "testOverParams" testOverParams
  , TestLabel "testBoundaries" testBoundaries
  , TestLabel "testPartitionArcAtTimes" testPartitionArcAtTimes
  , TestLabel "testPartitionAndGroupEventsAtBoundaries"
    testPartitionAndGroupEventsAtBoundaries
  , TestLabel "testMerge" testMerge
  , TestLabel "testMeta" testMeta
  , TestLabel "testMuseqNamesAreValid" testMuseqNamesAreValid
  , TestLabel "testIntNameEvents" testIntNameEvents
  , TestLabel "testNameAnonEvents" testNameAnonEvents
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
    $ (museq' 3 [] :: Museq ())
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ (museq' 0 [] :: Museq ())
  assertBool "valid, nonempty"                $ museqIsValid
    $ (museq' 1 [(0, New Boop "marge")])
  assertBool "invalid, time > _sup" $ not $ museqIsValid
    $ (museq' 1 [(1.5, New Boop "marge")])

testStack = TestCase $ do
  let y = museq 2 [((0,3),"y")]
      z = museq 3 [((1,2),"z")]
  assertBool "stack" $ stack y z ==
    L.set dur (_dur y) ( museq 6 [((0,3),"y")
                                 ,((1,2),"z")
                                 ,((2,5),"y")
                                 ,((4,5),"z")
                                 ,((4,7),"y") ] )
  assertBool "stack" $ stack (L.set dur 1 y) z ==
    L.set dur 1 ( museq 6 [((0,3),"y")
                           ,((1,2),"z")
                           ,((2,5),"y")
                           ,((4,5),"z")
                           ,((4,7),"y") ] )
  assertBool "stack, where timeToRepeat differs from timeToPlayThrough"
    $ stack (L.set sup 1 y) z ==
    L.set dur 2 ( museq 3 [((0,3),"y")
                           ,((1,2),"z")
                           ,((1,4),"y")
                           ,((2,5),"y") ] )

testRev = TestCase $ do
  let a = museq 2 [((0,   1),"a")
                  ,((1/3, 3),"b")
                  ,((1/2, 1/2),"c")]
  assertBool "rev" $ rev a == museq 2 [((0  , 1),"a")
                                      ,((3/2, 3/2),"c")
                                      ,((5/3, 13/3),"b")]

testEarlyAndLate = TestCase $ do
  let a = museq 10 [((0,11),"a"),((1,2),"b")]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [((0,1),"b"),((9,20),"a")]

  let a = museq 10 [((0,11),"a"),((1,2),"b")]
  assertBool "late" $ _vec (late 1 a) ==
    V.fromList [((1,12),"a"),((2,3),"b")]

testFastAndSlow = TestCase $ do
  let a = museq 10 [((0,20),"a"),((2,2),"b")]
  assertBool "fast" $ (fast 2 a) == museq 5 [((0,10),"a"),((1,1),"b")]
  assertBool "slow" $ (slow 2 a) == museq 20 [((0,40),"a"),((4,4),"b")]

testDenseAndSparse = TestCase $ do
  let x = museq 10 [((0,15),"a"),((2,2),"b")]
  assertBool "dense" $ dense 2 x ==
    L.set dur 10 (museq 5 [((0,15/2),"a"),((1,1),"b")])
  assertBool "sparse" $ sparse 2 x ==
    L.set dur 10 (museq 20 [((0,30),"a"),((4,4),"b")])

testExplicitReps = TestCase $ do
  let y = Museq {_dur = 3, _sup = 4
                , _vec = V.fromList [((0,3),()), ((1,1),())]}
  assertBool "explicitReps" $ explicitReps y ==
    [ V.fromList [((0,3),()), ((1,1),())]
    , V.fromList [((4,7),()), ((5,5),())] -- starts at 3
    , V.fromList [((8,11),())]         -- starts at 6
    , V.fromList [((9,9),())]         -- starts at 9
    ]
  assertBool "unsafeExplicitReps" $ unsafeExplicitReps 24 y ==
    [ V.fromList [((0,3),()), ((1,1),())]
    , V.fromList [((4,7),()), ((5,5),())]
    , V.fromList [((8,11),())]
    , V.fromList [((9,9),())]
    , V.fromList [((12,15),()), ((13,13),())]
    , V.fromList [((16,19),()), ((17,17),())]
    , V.fromList [((20,23),())]
    , V.fromList [((21,21),())]
    ]

testAppend = TestCase $ do
    let a = museq 1 [((0,1),"a")]
        a2  = a {_sup = RTime $ 2}
        a12 = a {_sup = RTime $ 1%2}
        a32 = a {_sup = RTime $ 3%2}
        b = museq 1 [((0,0),"b")]
    assertBool "testAppend" $ append a b ==
      museq 2 [((0,1),"a"),((1,1),"b")]
    assertBool "testAppend" $ append a2 b ==
      let m = museq 2 [((0,1),"a"),((1,1),"b"),((3,3),"b")]
      in m {_sup = 4}
    assertBool "testAppend" $ append a12 b ==
      museq 2 [((0,1),"a"),((1%2,3%2),"a"),((1,1),"b")]
    assertBool "testAppend" $ append a32 b ==
      let m = museq 2 [((0,1),"a"), ((1,1),"b"), ((2+1/2,3+1/2),"a")
                       , ((3,3),"b"), ((5,5),"b")]
      in m {_sup = 6}

testRep = TestCase $ do
  let a = museq 6 [((0,7),"a")]
  assertBool "rep int" $ rep 2 a ==
    L.set dur 12 (museq 6 [((0,7),"a")])
  assertBool "rep fraction" $ rep (3/2) a ==
    L.set dur 9 (museq 6 [((0,7),"a")])

testMapMuseqsDiff = TestCase $ do
  let msg = M.singleton "amp" 1
      m1 = M.fromList [("a", museq' 10 [(0, ("1", (Boop, msg)))])
                      ,("b", museq' 15 [(0, ("1", (Boop, msg)))
                                       ,(10,("2", (Boop, msg)))
                                      ] ) ]
      m2 = M.fromList [("a", museq' 10 [(0, ("2", (Vap, msg)))])
                      ,("b", museq' 15 [(0, ("2", (Boop, msg)))
                                       ,(10,("3", (Boop, msg)))
                                      ] ) ]
  assertBool "museqDiff" $ museqsDiff m1 m2 == ( [ (Boop,"1") ]
                                               , [ (Boop,"3")
                                                 , (Vap ,"2")
                                                 ] )
  assertBool "museqDiff" $ museqsDiff m2 m1 == ( [ (Boop,"3")
                                                 , (Vap ,"2") ]
                                               , [ (Boop,"1")
                                                 ] )

testArc = TestCase $ do
  let m = museq 5 [((0,6),"a"),((2,4),"b")]
  -- arguments to arc : time0 tempoPeriod from to museq
  assertBool "arc 0" $ arc 100 2  200 210  m
    == [ ((200,202),"a")
       , ((200,210),"a")
       , ((204,208),"b")]
  assertBool "arc 1" $ arc 101 2  200 210  m
    == [ ((200,203),"a")
       , ((201,210),"a")
       , ((205,209),"b")]
  assertBool "arc 1" $ arc 101 2  200 220  m
    == [ ((200,203),"a")
       , ((201,213),"a")
       , ((205,209),"b")
       , ((211,220),"a")
       , ((215,219),"b")]

m = museq' 2 [ (0, M.singleton "freq" 100)
             , (1, M.singleton "amp"  0.1) ] :: Museq Msg

testOverParams = TestCase $ do
  let m = museq' 2 [ (0, M.singleton "freq" 100)
                   , (1, M.singleton "amp"  0.1) ]
  assertBool "overParams" $ overParams [("freq",(+1))] m
    == museq' 2 [ (0, M.singleton "freq" 101)
                , (1, M.singleton "amp" 0.1)]
  assertBool "switchParams" $ switchParams [("freq","guzzle")] m
    == museq' 2 [ (0, M.singleton "guzzle" 100)
                , (1, M.singleton "amp" 0.1)]
  assertBool "keepParams" $ keepParams ["freq"] m
    == museq' 2 [(0, M.singleton "freq" 100)]
  assertBool "dropParams" $ dropParams ["freq"] m
    == museq' 2 [(1, M.singleton "amp" 0.1)]

testBoundaries = TestCase $ do
  assertBool "boundaries" $ boundaries [(0,1),(1,1),(2,3)]
    == [0,1,1,2,3]

testPartitionArcAtTimes = TestCase $ do
  assertBool "partitionArcAtTimes" $ partitionArcAtTimes [0,2,2,5,10] (0,5)
    == [(0,2),(2,2),(2,5)]

testPartitionAndGroupEventsAtBoundaries = TestCase $ do
  assertBool "partitionAndGroupEventsAtBoundaries" $
    partitionAndGroupEventsAtBoundaries [0, 1, 1, 2, 3, 4] [ ((0,3),"a")
                                                           , ((2,4),"b") ]
    == [((0,1),"a")
       ,((1,1),"a")
       ,((1,2),"a")
       ,((2,3),"a")
       ,((2,3),"b")
       ,((3,4),"b")
       ]

testMerge = TestCase $ do
  let a  = Museq {_dur = 2, _sup = 2, _vec = V.fromList [ ((0,1),"a") ] }
      bc = Museq {_dur = 3, _sup = 3, _vec = V.fromList [ ((0,1),"b")
                                                        , ((1,2),"c") ] }
      op = Museq {_dur = 3, _sup = 1.5, _vec = V.singleton ((0,1),(++) " ") }
  assertBool "merge" $ merge (++) a bc
    == Museq {_dur = 3, _sup = 6,
              _vec = V.fromList [ ((0,1),"ab")
                                , ((4,5),"ac") ] }
  assertBool "apply" $ (op <*> bc)
    == Museq {_dur = 3, _sup = 3,
              _vec = V.fromList [ ((0,1),  " b")
                                , ((1.5,2)," c")
                                ] }

  let a' = Museq {_dur = 2, _sup = 2,
                 _vec = V.fromList [ ((0,1), M.fromList [("amp",2)
                                                        ,("freq",2)] ) ] }
      bc' = Museq {_dur = 3, _sup = 3,
                 _vec = V.fromList [ ((0,1), M.singleton "amp" 0)
                                   , ((1,2), M.singleton "freq" 0) ] }
  assertBool "merge0" $ merge0 a' bc'
    == Museq {_dur = 3, _sup = 6,
              _vec = V.fromList [ ((0,1),M.fromList [("amp",2)
                                                    ,("freq",2)])
                                , ((4,5),M.fromList [("amp",2)
                                                    ,("freq",2)]) ] }
  assertBool "mergea" $ mergea a' bc'
    == Museq {_dur = 3, _sup = 6,
              _vec = V.fromList [ ((0,1),M.fromList [("amp",2)
                                                    ,("freq",2)])
                                , ((4,5),M.fromList [("amp",2)
                                                    ,("freq",0)]) ] }

testMeta = TestCase $ do
  let a = Museq {_dur = 2, _sup = 2, _vec = V.fromList [ ((0,1),"a") ] }
      f = Museq {_dur = 3, _sup = 3, _vec = V.fromList [ ((0,1), fast 2)
                                                       , ((2,3), early $ 1/4)
                                                       ] }
  assertBool "meta" $ meta f a
    == Museq {_dur = 2, _sup = 6,
              _vec = V.fromList [((0,    0.5),"a")
                                ,((2,    2.75),"a")
                                ,((3,    3.5),"a")
                                ,((5.75, 6),"a")]}

testMuseqNamesAreValid = TestCase $ do
  assertBool "empty Museq has valid names" $
    museqMaybeNamesAreValid $ museq 10 ([] :: [((Rational,Rational)
                                          , (Maybe String,()))])
  assertBool "Museq without names has valid names" $
    museqMaybeNamesAreValid $ museq 10 [ ((0, 10), (Nothing :: Maybe String,()))
                                  , ((0, 10), (Nothing,())) ]
  assertBool "Museq with overlapping like names is not valid" $ not $
    museqMaybeNamesAreValid $ museq 10 [ ((0,  6), (Just "1",()))
                                  , ((4, 10), (Just "1",())) ]
  assertBool "Museq with non-overlapping like names is valid" $
    museqMaybeNamesAreValid $ museq 10 [ ((0,  4), (Just "1",()))
                                  , ((6, 10), (Just "1",())) ]
  assertBool "Museq with overlapping unlike names is valid" $
    museqMaybeNamesAreValid $ museq 10 [ ((0,  6), (Just "1",()))
                                  , ((4, 10), (Just "2",())) ]
  assertBool "Museq with wrapped overlap is not valid" $ not $
    museqMaybeNamesAreValid $ museq 10 [ ((0,  4), (Just "1",()))
                                  , ((6, 12), (Just "1",())) ]

testIntNameEvents = TestCase $ do
  assertBool "intNameEvents" $
    intNameEvents 10 [((0,1), ()),      ((2,3), ())]
    ==               [((0,1), (1,())),  ((2,3), (1,()))]
  assertBool "intNameEvents" $
    intNameEvents 10 [((0,2), ()),      ((2,3), ())]
    ==               [((0,2), (1,())),  ((2,3), (2,()))]
  assertBool "intNameEvents" $
    intNameEvents 10 [((0,2), ()),      ((5,11), ())]
    ==               [((0,2), (1,())),  ((5,11), (2,()))]

testNameAnonEvents = TestCase $ do
  let m = museq 10 [((0,1),(Just "1",()))
                   ,((0,1),(Nothing, ()))]
  assertBool "testNameAnonEvents" $ nameAnonEvents m
    ==    museq 10 [((0,1),("1",()))
                   ,((0,1),("a1",()))]
