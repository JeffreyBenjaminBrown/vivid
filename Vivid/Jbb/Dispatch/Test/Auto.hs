module Vivid.Jbb.Dispatch.Test.Auto where

import qualified Control.Lens as L
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid.Jbb.Util hiding (m1)
import Vivid.Jbb.Synths
import Vivid.Jbb.Dispatch.Join
import Vivid.Jbb.Dispatch.Internal.Join
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Transform
import Vivid.Jbb.Dispatch.Types
import Test.HUnit


tests :: IO Counts
tests = runTestTT $ TestList
  [ TestLabel "testOverlap" testOverlap
  , TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid'" testMuseqIsValid'
  , TestLabel "testStack'" testStack'
  , TestLabel "testRev'" testRev'
  , TestLabel "testEarlyAndLate'" testEarlyAndLate'
  , TestLabel "testFastAndSlow'" testFastAndSlow'
  , TestLabel "testDenseAndSparse'" testDenseAndSparse'
  , TestLabel "testAppend'" testAppend'
  , TestLabel "testRep'" testRep'
  , TestLabel "testMuseqsDiff'" testMuseqsDiff'
  , TestLabel "testArc'" testArc'
  , TestLabel "testOverParams'" testOverParams'
  , TestLabel "testBoundaries" testBoundaries
  , TestLabel "testPartitionArcAtTimes" testPartitionArcAtTimes
  , TestLabel "testMerge'" testMerge'
  , TestLabel "testMeta'" testMeta'
  , TestLabel "testMuseqNamesAreValid'" testMuseqNamesAreValid'
  , TestLabel "testIntNameEvents'" testIntNameEvents'
  , TestLabel "testNameAnonEvents'" testNameAnonEvents'
  , TestLabel "testMultiPartition" testMultiPartition
  , TestLabel "testHold" testHold
  ]

testHold :: Test
testHold = TestCase $ do
  assertBool "1" $ hold 10 [(2,"a"), (7::Int,"b")]
    == [((2,7),"a"), ((7,12),"b")]

testMultiPartition :: Test
testMultiPartition = TestCase $ do
  assertBool "1" $ multiPartition [(1,2), (1,3)
                                  ,(2,1), (2::Int, 0::Int)]
                               == [(1, [2,3])
                                  ,(2, [1,0])]

testOverlap :: Test
testOverlap = TestCase $ do
  assertBool "no overlap" $ not
    $ overlap (0,1) (2,3::Int)
  assertBool "no overlap" $ not
    $ overlap (2,3) (0,1::Int)
  assertBool "one inside"
    $ overlap (0,1) (-1,2::Int)
  assertBool "one inside"
    $ overlap (-1,2) (0,1::Int)
  assertBool "equal"
    $ overlap (0,2) (0,2::Int)
  assertBool "equal"
    $ overlap (0,2) (0,2::Int)
  assertBool "left-equal"
    $ overlap (0,1) (0,2::Int)
  assertBool "left-equal"
    $ overlap (0,2) (0,1::Int)
  assertBool "right-equal"
    $ overlap (1,2) (0,2::Int)
  assertBool "right-equal"
    $ overlap (0,2) (1,2::Int)
  assertBool "instantaneous, equal"
    $ overlap (0,0) (0,0::Int)
  assertBool "instantaneous, equal"
    $ overlap (0,0) (0,0::Int)
  assertBool "instantaneous, not equal" $ not
    $ overlap (0,0) (1,1::Int)
  assertBool "instantaneous, not equal" $ not
    $ overlap (1,1) (0,0::Int)
  assertBool "one instant, left-equal"
    $ overlap (0,0) (0,1::Int)
  assertBool "one instant, left-equal"
    $ overlap (0,1) (0,0::Int)
  assertBool "one instant, right-equal"
    $ overlap (1,1) (0,1::Int)
  assertBool "one instant, right-equal"
    $ overlap (0,1) (1,1::Int)
  assertBool "one instant, inside"
    $ overlap (1,1) (0,2::Int)
  assertBool "one instant, inside"
    $ overlap (0,2) (1,1::Int)
  assertBool "one instant, outside on right" $ not
    $ overlap (3,3) (0,2::Int)
  assertBool "one instant, outside on right" $ not
    $ overlap (0,2) (3,3::Int)
  assertBool "one instant, outside on left" $ not
    $ overlap (3,3) (10,12::Int)
  assertBool "one instant, outside on left" $ not
    $ overlap (10,12) (3,3::Int)

testPrevPhase0 :: Test
testPrevPhase0 = TestCase $ do
  assertBool "" $ prevPhase0 0 10 11 == (10 :: Double)
  assertBool "" $ prevPhase0 0 10 10 == (10 :: Double)
  assertBool "" $ prevPhase0 0 20 41 == (40 :: Double)
  assertBool "" $ prevPhase0 0 20 59 == (40 :: Double)

testNextPhase0 :: Test
testNextPhase0 = TestCase $ do
  assertBool "" $ nextPhase0 0 10 11 == (20 :: Double)
  assertBool "" $ nextPhase0 0 10 10 == (10 :: Double)
  assertBool "" $ nextPhase0 0 20 41 == (60 :: Double)
  assertBool "" $ nextPhase0 0 20 59 == (60 :: Double)

testMuseqIsValid' :: Test
testMuseqIsValid' = TestCase $ do
  assertBool "valid, empty"                   $ museqIsValid
    $ (mkMuseq 3 [] :: Museq String ())
  assertBool "invalid, zero length"     $ not $ museqIsValid
    $ (mkMuseq 0 [] :: Museq String ())
  assertBool "valid, nonempty"                $ museqIsValid
    $ mkMuseq 1 [mkEv0 "1" 0 () ]
  assertBool "invalid, time > _sup" $ not $ museqIsValid
    $ mkMuseq 1 [mkEv0 "1" 2 () ]

testStack' :: Test
testStack' = TestCase $ do
  let y = mkMuseq 2 [mkEv () 0 3 "()"]
      z = mkMuseq 3 [mkEv "z" 1 2 "z"]
  assertBool "stack" $ stack y z ==
    L.set dur (_dur y) ( mkMuseq 6 [ mkEv "()"  0 3 "()"
                                      , mkEv "az" 1 2 "z"
                                      , mkEv "()"  2 5 "()"
                                      , mkEv "az" 4 5 "z"
                                      , mkEv "()"  4 7 "()"] )
  assertBool "stack" $ stack (L.set dur 1 y) z ==
    L.set dur 1 ( mkMuseq 6 [ mkEv "()"  0 3 "()"
                              , mkEv "az" 1 2 "z"
                              , mkEv "()"  2 5 "()"
                              , mkEv "az" 4 5 "z"
                              , mkEv "()"  4 7 "()" ] )
  assertBool "stack, where timeToRepeat differs from timeToPlayThrough"
    $ stack (L.set sup 1 y) z ==
    L.set dur 2 ( mkMuseq 3 [ mkEv "()"  0 3 "()"
                              , mkEv "az" 1 2 "z"
                              , mkEv "()"  1 4 "()"
                              , mkEv "()"  2 5 "()" ] )

testRev' :: Test
testRev' = TestCase $ do
  let a = mkMuseq 2 [ mkEv () 0     1     "a"
                   , mkEv () (1/3) 3     "b"
                   , mkEv () (1/2) (1/2) "c" ]
  assertBool "rev" $ rev a == mkMuseq 2 [ mkEv () 0     1      "a"
                                        , mkEv () (3/2) (3 /2)  "c"
                                        , mkEv () (5/3) (13/3) "b" ]

testEarlyAndLate' :: Test
testEarlyAndLate' = TestCase $ do
  let a = mkMuseq 10 [ mkEv () 0 11 "a"
                      , mkEv () 1 2 "b"]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [ mkEv () 0 1 "b"
               , mkEv () 9 20 "a"]

  let a' = mkMuseq 10 [ mkEv () 0 11 "a"
                       , mkEv () 1 2 "b"]
  assertBool "late" $ _vec (late 1 a') ==
    V.fromList [ mkEv () 1 12 "a"
               , mkEv () 2 3 "b"]

testFastAndSlow' :: Test
testFastAndSlow' = TestCase $ do
  let a = mkMuseq 10 [mkEv () 0 20 "a",mkEv () 2 2 "b"]
  assertBool "fast" $ (fast 2 a) == mkMuseq 5 [mkEv () 0 10 "a",mkEv () 1 1 "b"]
  assertBool "slow" $ (slow 2 a) == mkMuseq 20 [mkEv () 0 40 "a",mkEv () 4 4 "b"]

testDenseAndSparse' :: Test
testDenseAndSparse' = TestCase $ do
  let x = mkMuseq 10 [mkEv () 0 15 "a",mkEv () 2 2 "b"]
  assertBool "dense" $ dense 2 x ==
    L.set dur 10 (mkMuseq 5 [mkEv () 0 (15/2) "a",mkEv () 1 1 "b"])
  assertBool "sparse" $ sparse 2 x ==
    L.set dur 10 (mkMuseq 20 [mkEv () 0 30 "a",mkEv () 4 4 "b"])

-- This was written for the old Museq, where labels were attached
-- in the wrong place. That was changed in the "one-museq" branch,
-- but the test was not updated to use the new type.
--testExplicitReps :: Test
--testExplicitReps = TestCase $ do
--  let y = Museq {_dur = 3, _sup = 4
--                , _vec = V.fromList [((0,3),()), ((1,1),())]}
--  assertBool "explicitReps" $ explicitReps y ==
--    [ V.fromList [((0,3),()), ((1,1),())]
--    , V.fromList [((4,7),()), ((5,5),())] -- starts at 3
--    , V.fromList [((8,11),())]         -- starts at 6
--    , V.fromList [((9,9),())]         -- starts at 9
--    ]
--  assertBool "unsafeExplicitReps" $ unsafeExplicitReps 24 y ==
--    [ V.fromList [((0,3),()), ((1,1),())]
--    , V.fromList [((4,7),()), ((5,5),())]
--    , V.fromList [((8,11),())]
--    , V.fromList [((9,9),())]
--    , V.fromList [((12,15),()), ((13,13),())]
--    , V.fromList [((16,19),()), ((17,17),())]
--    , V.fromList [((20,23),())]
--    , V.fromList [((21,21),())]
--    ]

testAppend' :: Test
testAppend' = TestCase $ do
    let a = mkMuseq 1 [mkEv () 0 1 "a"]
        a2  = a {_sup = RTime $ 2}
        a12 = a {_sup = RTime $ 1%2}
        a32 = a {_sup = RTime $ 3%2}
        b = mkMuseq 1 [mkEv () 0 0 "b"]
    assertBool "testAppend" $ append a b ==
      mkMuseq 2 [mkEv () 0 1 "a", mkEv () 1 1 "b"]
    assertBool "testAppend'" $ append a2 b ==
      let m = mkMuseq 2 [mkEv () 0 1 "a",mkEv () 1 1 "b",mkEv () 3 3 "b"]
      in m {_sup = 4}
    assertBool "testAppend'" $ append a12 b ==
      mkMuseq 2 [mkEv () 0 1 "a",mkEv () (1%2) (3%2) "a", mkEv () 1 1 "b"]
    assertBool "testAppend'" $ append a32 b ==
      let m = mkMuseq 2 [ mkEv () 0 1 "a", mkEv () 1 1 "b"
                       , mkEv () (2+1/2) (3+1/2) "a"
                       , mkEv () 3 3 "b", mkEv () 5 5 "b"]
      in m {_sup = 6}

testRep' :: Test
testRep' = TestCase $ do
  let a = mkMuseq 6 [mkEv () 0 7 "a"]
  assertBool "rep int" $ rep 2 a ==
    L.set dur 12 (mkMuseq 6 [mkEv () 0 7 "a"])
  assertBool "rep fraction" $ rep (3/2) a ==
    L.set dur 9 (mkMuseq 6 [mkEv () 0 7 "a"])

testMuseqsDiff' :: Test
testMuseqsDiff' = TestCase $ do
  let msg = M.singleton "amp" 1
      m1 = M.fromList [("a", mkMuseq 10 [ mkEv0 "1" 0  (Note Boop msg)])
                      ,("b", mkMuseq 15 [ mkEv0 "1" 0  (Note Boop msg)
                                       , mkEv0 "2" 10 (Note Boop msg)
                                       ] ) ]
      m2 = M.fromList [("a", mkMuseq 10 [ mkEv0 "2" 0  (Note Vap msg) ])
                      ,("b", mkMuseq 15 [ mkEv0 "2" 0  (Note Boop msg)
                                       , mkEv0 "3" 10 (Note Boop msg)
                                       ] ) ]
  assertBool "museqDiff" $ museqsDiff m1 m2 == ( [ (Boop,"1") ]
                                               , [ (Boop,"3")
                                                 , (Vap ,"2")
                                                 ] )
  assertBool "museqDiff" $ museqsDiff m2 m1 == ( [ (Boop,"3")
                                                 , (Vap ,"2") ]
                                               , [ (Boop,"1")
                                                 ] )

testArc' :: Test
testArc' = TestCase $ do
  let m = mkMuseq 5 [ Event () (0,6) "a"
                   , Event () (2,4) "b"]
  -- arguments to arc : time0 tempoPeriod from to museq
  assertBool "arc 0" $ arc 100 2  200 210  m
    == [ ( Event () (200,202) "a" )
       , ( Event () (200,210) "a" )
       , ( Event () (204,208) "b" )]
  assertBool "arc 1" $ arc 101 2  200 210  m
    == [ ( Event () (200,203) "a")
       , ( Event () (201,210) "a")
       , ( Event () (205,209) "b")]
  assertBool "arc 1" $ arc 101 2  200 220  m
    == [ ( Event () (200,203) "a")
       , ( Event () (201,213) "a")
       , ( Event () (205,209) "b")
       , ( Event () (211,220) "a")
       , ( Event () (215,219) "b")]

testOverParams' :: Test
testOverParams' = TestCase $ do
  let m = mkMuseq 2 [ mkEv0 () 0 $ M.singleton "freq" 100
                   , mkEv0 () 1 $ M.singleton "amp"  0.1 ]
  assertBool "overParams" $ overParams [("freq",(+1))] m
    == mkMuseq 2 [ mkEv0 () 0 $ M.singleton "freq" 101
                , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "switchParams" $ switchParams [("freq","guzzle")] m
    == mkMuseq 2 [ mkEv0 () 0 $ M.singleton "guzzle" 100
                , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "keepParams" $ keepParams ["freq"] m
    == mkMuseq 2 [mkEv0 () 0 $ M.singleton "freq" 100]
  assertBool "dropParams" $ dropParams ["freq"] m
    == mkMuseq 2 [mkEv0 () 1 $ M.singleton "amp" 0.1]

testBoundaries :: Test
testBoundaries = TestCase $ do
  assertBool "boundaries" $ boundaries [(0,1),(1,1),(2,3::Int)]
    == [0,1,1,2,3]

testPartitionArcAtTimes :: Test
testPartitionArcAtTimes = TestCase $ do
  assertBool "partitionArcAtTimes" $ partitionArcAtTimes [0,2,2,5,10::Int]
    (0,5)
    == [(0,2),(2,2),(2,5)]

-- This was written for the old Museq, where labels were attached
-- in the wrong place. That was changed in the "one-museq" branch,
-- but the test was not updated to use the new type.
--testPartitionAndGroupEventsAtBoundaries :: Test
--testPartitionAndGroupEventsAtBoundaries = TestCase $ do
--  assertBool "partitionAndGroupEventsAtBoundaries" $
--    partitionAndGroupEventsAtBoundaries [0, 1, 1, 2, 3, 4 :: Int]
--      [ ((0,3),"a")
--      , ((2,4),"b") ]
--    == [((0,1),"a")
--       ,((1,1),"a")
--       ,((1,2),"a")
--       ,((2,3),"a")
--       ,((2,3),"b")
--       ,((3,4),"b")
--       ]

testMerge' :: Test
testMerge' = TestCase $ do
  let a  = Museq { _dur = 2, _sup = 2,
                    _vec = V.fromList [ mkEv "a" 0 1 "a" ] }
      bc = Museq { _dur = 3, _sup = 3,
                    _vec = V.fromList [ mkEv "b" 0 1 "b"
                                       , mkEv "c" 1 2 "c" ] }
      op = Museq { _dur = 3, _sup = 1.5,
                    _vec = V.singleton $ mkEv "op" 0 1 $ (++) "-" }
  assertBool "merge" $ merge (++) a bc
    == Museq { _dur = 3, _sup = 6,
                _vec = V.fromList [ mkEv "ab" 0 1 "ab"
                                   , mkEv "ac" 4 5 "ac" ] }
  assertBool "apply" $ (labelsToStrings op <*> labelsToStrings bc)
    == Museq { _dur = 3, _sup = 3,
                _vec = V.fromList [ mkEv "opb" 0 1  "-b"
                                   , mkEv "opc" 1.5 2  "-c" ] }

  let a' = Museq { _dur = 2, _sup = 2,
                    _vec = V.fromList [ mkEv "a" 0 1
                                         $ M.fromList [ ("amp",2)
                                                      , ("freq",2)] ] }
      bc' = Museq { _dur = 3, _sup = 3,
                     _vec = V.fromList [ mkEv "b" 0 1 $ M.singleton "amp" 0
                                        , mkEv "c" 1 2 $ M.singleton "freq" 0] }
  assertBool "merge0" $ merge0 a' bc'
    == Museq { _dur = 3, _sup = 6,
                _vec = V.fromList [ mkEv "ab" 0 1 $ M.fromList [("amp",2)
                                                              ,("freq",2)]
                                   , mkEv "ac" 4 5 $ M.fromList [("amp",2)
                                                              ,("freq",2)] ] }
  assertBool "mergea" $ mergea a' bc'
    == Museq { _dur = 3, _sup = 6,
                _vec = V.fromList [ mkEv "ab" 0 1 $ M.fromList [("amp",2)
                                                              ,("freq",2)]
                                   , mkEv "ac" 4 5 $ M.fromList [("amp",2)
                                                              ,("freq",0)] ] }

testMeta' :: Test
testMeta' = TestCase $ do
  let a = Museq { _dur = 2, _sup = 2,
                   _vec = V.fromList [ mkEv "a" 0 1 "a" ] }
      f = Museq { _dur = 3, _sup = 3,
                   _vec = V.fromList [ mkEv "f" 0 1 $ fast 2
                                      , mkEv "g" 2 3 $ early $ 1/4 ] }
  assertBool "meta" $ meta f a
    == Museq { _dur = 2, _sup = 6,
                _vec = V.fromList [ mkEv "fa" 0     0.5 "a"
                                   , mkEv "ga" 2     2.75 "a"
                                   , mkEv "fa" 3     3.5 "a"
                                   , mkEv "ga" 5.75  6 "a" ] }

testMuseqNamesAreValid' :: Test
testMuseqNamesAreValid' = TestCase $ do
  assertBool "empty Museq has valid names" $ museqMaybeNamesAreValid $
    mkMuseq 10 ([] :: [Ev (Maybe String) ()])
  assertBool "Museq without names has valid names"
    $ museqMaybeNamesAreValid
    $ mkMuseq 10 [ mkEv (Nothing :: Maybe String) 0 10 ()
                , mkEv (Nothing :: Maybe String) 0 10 () ]
  assertBool "Museq with overlapping like names is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseq 10 [ mkEv (Just "1") 0  6 ()
                                         , mkEv (Just "1") 4 10 () ]
  assertBool "Museq with non-overlapping like names is valid" $
    museqMaybeNamesAreValid $ mkMuseq 10 [ mkEv (Just "1") 0 4  ()
                                         , mkEv (Just "1") 6 10 () ]
  assertBool "Museq with overlapping unlike names is valid" $
    museqMaybeNamesAreValid $ mkMuseq 10 [ mkEv (Just "1") 0 6 ()
                                         , mkEv (Just "2") 4 10 () ]
  assertBool "Museq with wrapped overlap is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseq 10 [ mkEv (Just "1") 0 4 ()
                                         , mkEv (Just "1") 6 12 () ]

testIntNameEvents' :: Test
testIntNameEvents' = TestCase $ do
  assertBool "intNameEvents, no overlap" $
    intNameEvents 10 [mkEv () 0  1 (),  mkEv () 2 3 ()]
    ==                [mkEv  1 0  1 (),  mkEv  1 2 3 ()]
  assertBool "intNameEvents, ordinary overlap" $
    intNameEvents 10 [mkEv () 0 2 (),  mkEv () 1 3 ()]
    ==                [mkEv  1 0 2 (),  mkEv  2 1 3 ()]
  assertBool "intNameEvents, wrapped overlap" $
    intNameEvents 10 [mkEv () 0 2 (),  mkEv () 5 11 ()]
    ==                [mkEv  1 0 2 (),  mkEv  2 5 11 ()]

testNameAnonEvents' :: Test
testNameAnonEvents' = TestCase $ do
  let m = mkMuseq 10 [ mkEv (Just "1") 0 1 ()
                    , mkEv Nothing    0 1 () ]
  assertBool "testNameAnonEvents'" $ nameAnonEvents m
    ==    mkMuseq 10 [ mkEv "1"  0 1 ()
                    , mkEv "a1" 0 1 () ]
