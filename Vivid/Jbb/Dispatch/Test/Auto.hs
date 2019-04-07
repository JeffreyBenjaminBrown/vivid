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
  [ TestLabel "testOverlap" testOverlap
  , TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "museqIsValid'" testMuseqIsValid'
  , TestLabel "testStack" testStack
  , TestLabel "testStack'" testStack'
  , TestLabel "testRev" testRev
  , TestLabel "testRev'" testRev'
  , TestLabel "testEarlyAndLate" testEarlyAndLate
  , TestLabel "testEarlyAndLate'" testEarlyAndLate'
  , TestLabel "testFastAndSlow" testFastAndSlow
  , TestLabel "testFastAndSlow'" testFastAndSlow'
  , TestLabel "testDenseAndSparse" testDenseAndSparse
  , TestLabel "testDenseAndSparse'" testDenseAndSparse'
  , TestLabel "testExplicitReps" testExplicitReps
  , TestLabel "testAppend" testAppend
  , TestLabel "testAppend'" testAppend'
  , TestLabel "testRep" testRep
  , TestLabel "testRep'" testRep'
  , TestLabel "testMuseqsDiff" testMuseqsDiff
  , TestLabel "testMuseqsDiff'" testMuseqsDiff'
  , TestLabel "testArc" testArc
  , TestLabel "testArc'" testArc'
  , TestLabel "testOverParams" testOverParams
  , TestLabel "testOverParams'" testOverParams'
  , TestLabel "testBoundaries" testBoundaries
  , TestLabel "testPartitionArcAtTimes" testPartitionArcAtTimes
  , TestLabel "testPartitionAndGroupEventsAtBoundaries"
    testPartitionAndGroupEventsAtBoundaries
  , TestLabel "testMerge" testMerge
  , TestLabel "testMerge'" testMerge'
  , TestLabel "testMeta" testMeta
  , TestLabel "testMeta'" testMeta'
  , TestLabel "testMuseqNamesAreValid" testMuseqNamesAreValid
  , TestLabel "testMuseqNamesAreValid'" testMuseqNamesAreValid'
  , TestLabel "testIntNameEvents" testIntNameEvents
  , TestLabel "testIntNameEvents'" testIntNameEvents'
  , TestLabel "testNameAnonEvents" testNameAnonEvents
  , TestLabel "testNameAnonEvents'" testNameAnonEvents'
  ]

testOverlap = TestCase $ do
  assertBool "no overlap" $ not
    $ overlap (0,1) (2,3)
  assertBool "no overlap" $ not
    $ overlap (2,3) (0,1)
  assertBool "one inside"
    $ overlap (0,1) (-1,2)
  assertBool "one inside"
    $ overlap (-1,2) (0,1)
  assertBool "equal"
    $ overlap (0,2) (0,2)
  assertBool "equal"
    $ overlap (0,2) (0,2)
  assertBool "left-equal"
    $ overlap (0,1) (0,2)
  assertBool "left-equal"
    $ overlap (0,2) (0,1)
  assertBool "right-equal"
    $ overlap (1,2) (0,2)
  assertBool "right-equal"
    $ overlap (0,2) (1,2)
  assertBool "instantaneous, equal"
    $ overlap (0,0) (0,0)
  assertBool "instantaneous, equal"
    $ overlap (0,0) (0,0)
  assertBool "instantaneous, not equal" $ not
    $ overlap (0,0) (1,1)
  assertBool "instantaneous, not equal" $ not
    $ overlap (1,1) (0,0)
  assertBool "one instant, left-equal"
    $ overlap (0,0) (0,1)
  assertBool "one instant, left-equal"
    $ overlap (0,1) (0,0)
  assertBool "one instant, right-equal"
    $ overlap (1,1) (0,1)
  assertBool "one instant, right-equal"
    $ overlap (0,1) (1,1)
  assertBool "one instant, inside"
    $ overlap (1,1) (0,2)
  assertBool "one instant, inside"
    $ overlap (0,2) (1,1)
  assertBool "one instant, outside on right" $ not
    $ overlap (3,3) (0,2)
  assertBool "one instant, outside on right" $ not
    $ overlap (0,2) (3,3)
  assertBool "one instant, outside on left" $ not
    $ overlap (3,3) (10,12)
  assertBool "one instant, outside on left" $ not
    $ overlap (10,12) (3,3)

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
    $ (mkMuseq0 3 [] :: Museq ())
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ (mkMuseq0 0 [] :: Museq ())
  assertBool "valid, nonempty"                $ museqIsValid
    $ (mkMuseq0 1 [(0, New Boop "marge")])
  assertBool "invalid, time > _sup" $ not $ museqIsValid
    $ (mkMuseq0 1 [(1.5, New Boop "marge")])

testMuseqIsValid' = TestCase $ do
  let vempty = V.empty :: V.Vector (RTime, ())
  assertBool "valid, empty"                   $ museqIsValid'
    $ (mkMuseq' 3 [] :: Museq' String ())
  assertBool "invalid, zero length"     $ not $ museqIsValid'
    $ (mkMuseq' 0 [] :: Museq' String ())
  assertBool "valid, nonempty"                $ museqIsValid'
    $ mkMuseq' 1 [mkEv0 "1" 0 () ]
  assertBool "invalid, time > _sup" $ not $ museqIsValid'
    $ mkMuseq' 1 [mkEv0 "1" 2 () ]

testStack = TestCase $ do
  let y = mkMuseq 2 [((0,3),"y")]
      z = mkMuseq 3 [((1,2),"z")]
  assertBool "stack" $ stack y z ==
    L.set dur (_dur y) ( mkMuseq 6 [((0,3),"y")
                                 ,((1,2),"z")
                                 ,((2,5),"y")
                                 ,((4,5),"z")
                                 ,((4,7),"y") ] )
  assertBool "stack" $ stack (L.set dur 1 y) z ==
    L.set dur 1 ( mkMuseq 6 [((0,3),"y")
                           ,((1,2),"z")
                           ,((2,5),"y")
                           ,((4,5),"z")
                           ,((4,7),"y") ] )
  assertBool "stack, where timeToRepeat differs from timeToPlayThrough"
    $ stack (L.set sup 1 y) z ==
    L.set dur 2 ( mkMuseq 3 [((0,3),"y")
                           ,((1,2),"z")
                           ,((1,4),"y")
                           ,((2,5),"y") ] )

testStack' = TestCase $ do
  let y = mkMuseq' 2 [mkEv () 0 3 "()"]
      z = mkMuseq' 3 [mkEv "z" 1 2 "z"]
  assertBool "stack" $ stack' y z ==
    L.set dur' (_dur' y) ( mkMuseq' 6 [ mkEv "()"  0 3 "()"
                                    , mkEv "az" 1 2 "z"
                                    , mkEv "()"  2 5 "()"
                                    , mkEv "az" 4 5 "z"
                                    , mkEv "()"  4 7 "()"] )
  assertBool "stack" $ stack' (L.set dur' 1 y) z ==
    L.set dur' 1 ( mkMuseq' 6 [ mkEv "()"  0 3 "()"
                            , mkEv "az" 1 2 "z"
                            , mkEv "()"  2 5 "()"
                            , mkEv "az" 4 5 "z"
                            , mkEv "()"  4 7 "()" ] )
  assertBool "stack, where timeToRepeat differs from timeToPlayThrough"
    $ stack' (L.set sup' 1 y) z ==
    L.set dur' 2 ( mkMuseq' 3 [ mkEv "()"  0 3 "()"
                            , mkEv "az" 1 2 "z"
                            , mkEv "()"  1 4 "()"
                            , mkEv "()"  2 5 "()" ] )

testRev = TestCase $ do
  let a = mkMuseq 2 [((0,   1),"a")
                  ,((1/3, 3),"b")
                  ,((1/2, 1/2),"c")]
  assertBool "rev" $ rev a == mkMuseq 2 [((0  , 1),"a")
                                      ,((3/2, 3/2),"c")
                                      ,((5/3, 13/3),"b")]

testRev' = TestCase $ do
  let a = mkMuseq' 2 [ mkEv () 0     1     "a"
                   , mkEv () (1/3) 3     "b"
                   , mkEv () (1/2) (1/2) "c" ]
  assertBool "rev" $ rev' a == mkMuseq' 2 [ mkEv () 0     1      "a"
                                        , mkEv () (3/2) (3 /2)  "c"
                                        , mkEv () (5/3) (13/3) "b" ]

testEarlyAndLate = TestCase $ do
  let a = mkMuseq 10 [((0,11),"a"),((1,2),"b")]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [((0,1),"b"),((9,20),"a")]

  let a = mkMuseq 10 [((0,11),"a"),((1,2),"b")]
  assertBool "late" $ _vec (late 1 a) ==
    V.fromList [((1,12),"a"),((2,3),"b")]

testEarlyAndLate' = TestCase $ do
  let a = mkMuseq' 10 [ mkEv () 0 11 "a"
                    , mkEv () 1 2 "b"]
  assertBool "early" $ _vec' (early' 1 a) ==
    V.fromList [ mkEv () 0 1 "b"
               , mkEv () 9 20 "a"]

  let a = mkMuseq' 10 [ mkEv () 0 11 "a"
                    , mkEv () 1 2 "b"]
  assertBool "late" $ _vec' (late' 1 a) ==
    V.fromList [ mkEv () 1 12 "a"
               , mkEv () 2 3 "b"]

testFastAndSlow = TestCase $ do
  let a = mkMuseq 10 [((0,20),"a"),((2,2),"b")]
  assertBool "fast" $ (fast 2 a) == mkMuseq 5 [((0,10),"a"),((1,1),"b")]
  assertBool "slow" $ (slow 2 a) == mkMuseq 20 [((0,40),"a"),((4,4),"b")]

testFastAndSlow' = TestCase $ do
  let a = mkMuseq' 10 [mkEv () 0 20 "a",mkEv () 2 2 "b"]
  assertBool "fast" $ (fast' 2 a) == mkMuseq' 5 [mkEv () 0 10 "a",mkEv () 1 1 "b"]
  assertBool "slow" $ (slow' 2 a) == mkMuseq' 20 [mkEv () 0 40 "a",mkEv () 4 4 "b"]

testDenseAndSparse = TestCase $ do
  let x = mkMuseq 10 [((0,15),"a"),((2,2),"b")]
  assertBool "dense" $ dense 2 x ==
    L.set dur 10 (mkMuseq 5 [((0,15/2),"a"),((1,1),"b")])
  assertBool "sparse" $ sparse 2 x ==
    L.set dur 10 (mkMuseq 20 [((0,30),"a"),((4,4),"b")])

testDenseAndSparse' = TestCase $ do
  let x = mkMuseq' 10 [mkEv () 0 15 "a",mkEv () 2 2 "b"]
  assertBool "dense" $ dense' 2 x ==
    L.set dur' 10 (mkMuseq' 5 [mkEv () 0 (15/2) "a",mkEv () 1 1 "b"])
  assertBool "sparse" $ sparse' 2 x ==
    L.set dur' 10 (mkMuseq' 20 [mkEv () 0 30 "a",mkEv () 4 4 "b"])

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
    let a = mkMuseq 1 [((0,1),"a")]
        a2  = a {_sup = RTime $ 2}
        a12 = a {_sup = RTime $ 1%2}
        a32 = a {_sup = RTime $ 3%2}
        b = mkMuseq 1 [((0,0),"b")]
    assertBool "testAppend" $ append a b ==
      mkMuseq 2 [((0,1),"a"),((1,1),"b")]
    assertBool "testAppend" $ append a2 b ==
      let m = mkMuseq 2 [((0,1),"a"),((1,1),"b"),((3,3),"b")]
      in m {_sup = 4}
    assertBool "testAppend" $ append a12 b ==
      mkMuseq 2 [((0,1),"a"),((1%2,3%2),"a"),((1,1),"b")]
    assertBool "testAppend" $ append a32 b ==
      let m = mkMuseq 2 [((0,1),"a"), ((1,1),"b"), ((2+1/2,3+1/2),"a")
                       , ((3,3),"b"), ((5,5),"b")]
      in m {_sup = 6}

testAppend' = TestCase $ do
    let a = mkMuseq' 1 [mkEv () 0 1 "a"]
        a2  = a {_sup' = RTime $ 2}
        a12 = a {_sup' = RTime $ 1%2}
        a32 = a {_sup' = RTime $ 3%2}
        b = mkMuseq' 1 [mkEv () 0 0 "b"]
    assertBool "testAppend" $ append' a b ==
      mkMuseq' 2 [mkEv () 0 1 "a", mkEv () 1 1 "b"]
    assertBool "testAppend'" $ append' a2 b ==
      let m = mkMuseq' 2 [mkEv () 0 1 "a",mkEv () 1 1 "b",mkEv () 3 3 "b"]
      in m {_sup' = 4}
    assertBool "testAppend'" $ append' a12 b ==
      mkMuseq' 2 [mkEv () 0 1 "a",mkEv () (1%2) (3%2) "a", mkEv () 1 1 "b"]
    assertBool "testAppend'" $ append' a32 b ==
      let m = mkMuseq' 2 [ mkEv () 0 1 "a", mkEv () 1 1 "b"
                       , mkEv () (2+1/2) (3+1/2) "a"
                       , mkEv () 3 3 "b", mkEv () 5 5 "b"]
      in m {_sup' = 6}

testRep = TestCase $ do
  let a = mkMuseq 6 [((0,7),"a")]
  assertBool "rep int" $ rep 2 a ==
    L.set dur 12 (mkMuseq 6 [((0,7),"a")])
  assertBool "rep fraction" $ rep (3/2) a ==
    L.set dur 9 (mkMuseq 6 [((0,7),"a")])

testRep' = TestCase $ do
  let a = mkMuseq' 6 [mkEv () 0 7 "a"]
  assertBool "rep int" $ rep' 2 a ==
    L.set dur' 12 (mkMuseq' 6 [mkEv () 0 7 "a"])
  assertBool "rep fraction" $ rep' (3/2) a ==
    L.set dur' 9 (mkMuseq' 6 [mkEv () 0 7 "a"])

testMuseqsDiff = TestCase $ do
  let msg = M.singleton "amp" 1
      m1 = M.fromList [("a", mkMuseq0 10 [(0, ("1", (Boop, msg)))])
                      ,("b", mkMuseq0 15 [(0, ("1", (Boop, msg)))
                                       ,(10,("2", (Boop, msg)))
                                      ] ) ]
      m2 = M.fromList [("a", mkMuseq0 10 [(0, ("2", (Vap, msg)))])
                      ,("b", mkMuseq0 15 [(0, ("2", (Boop, msg)))
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

testMuseqsDiff' = TestCase $ do
  let msg = M.singleton "amp" 1
      m1 = M.fromList [("a", mkMuseq' 10 [ mkEv0 "1" 0  (Note' Boop msg)])
                      ,("b", mkMuseq' 15 [ mkEv0 "1" 0  (Note' Boop msg)
                                       , mkEv0 "2" 10 (Note' Boop msg)
                                       ] ) ]
      m2 = M.fromList [("a", mkMuseq' 10 [ mkEv0 "2" 0  (Note' Vap msg) ])
                      ,("b", mkMuseq' 15 [ mkEv0 "2" 0  (Note' Boop msg)
                                       , mkEv0 "3" 10 (Note' Boop msg)
                                       ] ) ]
  assertBool "museqDiff" $ museqsDiff' m1 m2 == ( [ (Boop,"1") ]
                                               , [ (Boop,"3")
                                                 , (Vap ,"2")
                                                 ] )
  assertBool "museqDiff" $ museqsDiff' m2 m1 == ( [ (Boop,"3")
                                                 , (Vap ,"2") ]
                                               , [ (Boop,"1")
                                                 ] )

testArc = TestCase $ do
  let m = mkMuseq 5 [((0,6),"a"),((2,4),"b")]
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

testArc' = TestCase $ do
  let m = mkMuseq' 5 [ Event () (0,6) "a"
                   , Event () (2,4) "b"]
  -- arguments to arc : time0 tempoPeriod from to museq
  assertBool "arc 0" $ arc' 100 2  200 210  m
    == [ ( Event () (200,202) "a" )
       , ( Event () (200,210) "a" )
       , ( Event () (204,208) "b" )]
  assertBool "arc' 1" $ arc' 101 2  200 210  m
    == [ ( Event () (200,203) "a")
       , ( Event () (201,210) "a")
       , ( Event () (205,209) "b")]
  assertBool "arc' 1" $ arc' 101 2  200 220  m
    == [ ( Event () (200,203) "a")
       , ( Event () (201,213) "a")
       , ( Event () (205,209) "b")
       , ( Event () (211,220) "a")
       , ( Event () (215,219) "b")]

testOverParams = TestCase $ do
  let m = mkMuseq0 2 [ (0, M.singleton "freq" 100)
                   , (1, M.singleton "amp"  0.1) ]
  assertBool "overParams" $ overParams [("freq",(+1))] m
    == mkMuseq0 2 [ (0, M.singleton "freq" 101)
                , (1, M.singleton "amp" 0.1)]
  assertBool "switchParams" $ switchParams [("freq","guzzle")] m
    == mkMuseq0 2 [ (0, M.singleton "guzzle" 100)
                , (1, M.singleton "amp" 0.1)]
  assertBool "keepParams" $ keepParams ["freq"] m
    == mkMuseq0 2 [(0, M.singleton "freq" 100)]
  assertBool "dropParams" $ dropParams ["freq"] m
    == mkMuseq0 2 [(1, M.singleton "amp" 0.1)]

testOverParams' = TestCase $ do
  let m = mkMuseq' 2 [ mkEv0 () 0 $ M.singleton "freq" 100
                   , mkEv0 () 1 $ M.singleton "amp"  0.1 ]
  assertBool "overParams'" $ overParams' [("freq",(+1))] m
    == mkMuseq' 2 [ mkEv0 () 0 $ M.singleton "freq" 101
                , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "switchParams'" $ switchParams' [("freq","guzzle")] m
    == mkMuseq' 2 [ mkEv0 () 0 $ M.singleton "guzzle" 100
                , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "keepParams'" $ keepParams' ["freq"] m
    == mkMuseq' 2 [mkEv0 () 0 $ M.singleton "freq" 100]
  assertBool "dropParams'" $ dropParams' ["freq"] m
    == mkMuseq' 2 [mkEv0 () 1 $ M.singleton "amp" 0.1]

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

a  = Museq' { _dur' = 2, _sup' = 2,
              _vec' = V.fromList [ mkEv "a" 0 1 "a" ] }
bc = Museq' { _dur' = 3, _sup' = 3,
              _vec' = V.fromList [ mkEv "b" 0 1 "b"
                                 , mkEv "c" 1 2 "c" ] }
op = Museq' { _dur' = 3, _sup' = 1.5,
              _vec' = V.singleton $ mkEv "op" 0 1 $ (++) " " }

testMerge' = TestCase $ do
  let a  = Museq' { _dur' = 2, _sup' = 2,
                    _vec' = V.fromList [ mkEv "a" 0 1 "a" ] }
      bc = Museq' { _dur' = 3, _sup' = 3,
                    _vec' = V.fromList [ mkEv "b" 0 1 "b"
                                       , mkEv "c" 1 2 "c" ] }
      op = Museq' { _dur' = 3, _sup' = 1.5,
                    _vec' = V.singleton $ mkEv "op" 0 1 $ (++) "-" }
  assertBool "merge" $ merge' (++) a bc
    == Museq' { _dur' = 3, _sup' = 6,
                _vec' = V.fromList [ mkEv "ab" 0 1 "ab"
                                   , mkEv "ac" 4 5 "ac" ] }
  assertBool "apply" $ (labelsToStrings op <*> labelsToStrings bc)
    == Museq' { _dur' = 3, _sup' = 3,
                _vec' = V.fromList [ mkEv "opb" 0 1  "-b"
                                   , mkEv "opc" 1.5 2  "-c" ] }

  let a' = Museq' { _dur' = 2, _sup' = 2,
                    _vec' = V.fromList [ mkEv "a" 0 1
                                         $ M.fromList [ ("amp",2)
                                                      , ("freq",2)] ] }
      bc' = Museq' { _dur' = 3, _sup' = 3,
                     _vec' = V.fromList [ mkEv "b" 0 1 $ M.singleton "amp" 0
                                        , mkEv "c" 1 2 $ M.singleton "freq" 0] }
  assertBool "merge0" $ merge0' a' bc'
    == Museq' { _dur' = 3, _sup' = 6,
                _vec' = V.fromList [ mkEv "ab" 0 1 $ M.fromList [("amp",2)
                                                              ,("freq",2)]
                                   , mkEv "ac" 4 5 $ M.fromList [("amp",2)
                                                              ,("freq",2)] ] }
  assertBool "mergea" $ mergea' a' bc'
    == Museq' { _dur' = 3, _sup' = 6,
                _vec' = V.fromList [ mkEv "ab" 0 1 $ M.fromList [("amp",2)
                                                              ,("freq",2)]
                                   , mkEv "ac" 4 5 $ M.fromList [("amp",2)
                                                              ,("freq",0)] ] }

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

testMeta' = TestCase $ do
  let a = Museq' { _dur' = 2, _sup' = 2,
                   _vec' = V.fromList [ mkEv "a" 0 1 "a" ] }
      f = Museq' { _dur' = 3, _sup' = 3,
                   _vec' = V.fromList [ mkEv "f" 0 1 $ fast' 2
                                      , mkEv "g" 2 3 $ early' $ 1/4 ] }
  assertBool "meta" $ meta' f a
    == Museq' { _dur' = 2, _sup' = 6,
                _vec' = V.fromList [ mkEv "fa" 0     0.5 "a"
                                   , mkEv "ga" 2     2.75 "a"
                                   , mkEv "fa" 3     3.5 "a"
                                   , mkEv "ga" 5.75  6 "a" ] }

testMuseqNamesAreValid = TestCase $ do
  assertBool "empty Museq has valid names" $
    museqMaybeNamesAreValid $ mkMuseq 10 ([] :: [((Rational,Rational)
                                          , (Maybe String,()))])
  assertBool "Museq without names has valid names" $
    museqMaybeNamesAreValid $ mkMuseq 10 [ ((0, 10), (Nothing :: Maybe String,()))
                                  , ((0, 10), (Nothing,())) ]
  assertBool "Museq with overlapping like names is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseq 10 [ ((0,  6), (Just "1",()))
                                  , ((4, 10), (Just "1",())) ]
  assertBool "Museq with non-overlapping like names is valid" $
    museqMaybeNamesAreValid $ mkMuseq 10 [ ((0,  4), (Just "1",()))
                                  , ((6, 10), (Just "1",())) ]
  assertBool "Museq with overlapping unlike names is valid" $
    museqMaybeNamesAreValid $ mkMuseq 10 [ ((0,  6), (Just "1",()))
                                  , ((4, 10), (Just "2",())) ]
  assertBool "Museq with wrapped overlap is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseq 10 [ ((0,  4), (Just "1",()))
                                  , ((6, 12), (Just "1",())) ]

testMuseqNamesAreValid' = TestCase $ do
  assertBool "empty Museq' has valid names" $ museqMaybeNamesAreValid' $
    mkMuseq' 10 ([] :: [Ev' (Maybe String) ()])
  assertBool "Museq' without names has valid names"
    $ museqMaybeNamesAreValid'
    $ mkMuseq' 10 [ mkEv (Nothing :: Maybe String) 0 10 ()
                , mkEv (Nothing :: Maybe String) 0 10 () ]
  assertBool "Museq' with overlapping like names is not valid" $ not $
    museqMaybeNamesAreValid' $ mkMuseq' 10 [ mkEv (Just "1") 0  6 ()
                                         , mkEv (Just "1") 4 10 () ]
  assertBool "Museq' with non-overlapping like names is valid" $
    museqMaybeNamesAreValid' $ mkMuseq' 10 [ mkEv (Just "1") 0 4  ()
                                         , mkEv (Just "1") 6 10 () ]
  assertBool "Museq' with overlapping unlike names is valid" $
    museqMaybeNamesAreValid' $ mkMuseq' 10 [ mkEv (Just "1") 0 6 ()
                                         , mkEv (Just "2") 4 10 () ]
  assertBool "Museq' with wrapped overlap is not valid" $ not $
    museqMaybeNamesAreValid' $ mkMuseq' 10 [ mkEv (Just "1") 0 4 ()
                                         , mkEv (Just "1") 6 12 () ]

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

testIntNameEvents' = TestCase $ do
  assertBool "intNameEvents', no overlap" $
    intNameEvents' 10 [mkEv () 0  1 (),  mkEv () 2 3 ()]
    ==                [mkEv  1 0  1 (),  mkEv  1 2 3 ()]
  assertBool "intNameEvents', ordinary overlap" $
    intNameEvents' 10 [mkEv () 0 2 (),  mkEv () 1 3 ()]
    ==                [mkEv  1 0 2 (),  mkEv  2 1 3 ()]
  assertBool "intNameEvents', wrapped overlap" $
    intNameEvents' 10 [mkEv () 0 2 (),  mkEv () 5 11 ()]
    ==                [mkEv  1 0 2 (),  mkEv  2 5 11 ()]

testNameAnonEvents = TestCase $ do
  let m = mkMuseq 10 [((0,1),(Just "1",()))
                   ,((0,1),(Nothing, ()))]
  assertBool "testNameAnonEvents" $ nameAnonEvents m
    ==    mkMuseq 10 [((0,1),("1",()))
                   ,((0,1),("a1",()))]

testNameAnonEvents' = TestCase $ do
  let m = mkMuseq' 10 [ mkEv (Just "1") 0 1 ()
                    , mkEv Nothing    0 1 () ]
  assertBool "testNameAnonEvents'" $ nameAnonEvents' m
    ==    mkMuseq' 10 [ mkEv "1"  0 1 ()
                    , mkEv "a1" 0 1 () ]
