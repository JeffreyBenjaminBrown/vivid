{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.Test.Dispatch where

import Test.HUnit

import           Control.Lens hiding (op)
import qualified Data.Map as M
import           Data.Ratio
import qualified Data.Vector as V

import Montevideo.Dispatch.Abbrevs
import Montevideo.Dispatch.Join
import Montevideo.Dispatch.Join.Internal
import Montevideo.Dispatch.Museq
import Montevideo.Dispatch.Museq.Mk
import Montevideo.Dispatch.Time
import Montevideo.Dispatch.Transform
import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Util


tests :: Test
tests = TestList [
    TestLabel "testOverlap" testOverlap
  , TestLabel "testPrevPhase0" testPrevPhase0
  , TestLabel "testNextPhase0" testNextPhase0
  , TestLabel "museqIsValid" testMuseqIsValid
  , TestLabel "testStack" testStack
  , TestLabel "testRev" testRev
  , TestLabel "testEarlyAndLate" testEarlyAndLate
  , TestLabel "testFastAndSlow" testFastAndSlow
  , TestLabel "testDenseAndSparse" testDenseAndSparse
  , TestLabel "testExplicitReps" testExplicitReps
  , TestLabel "testAppend" testAppend
  , TestLabel "testCat" testCat
  , TestLabel "testRep" testRep
  , TestLabel "testMuseqsDiff" testMuseqsDiff
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
  , TestLabel "testMultiPartition" testMultiPartition
  , TestLabel "testHold" testHold
  , TestLabel "test_timeToFinish" test_timeToFinish
  , TestLabel "test_separateVoices" test_separateVoices
  , TestLabel "test_gaps" test_gaps
  ]


test_gaps :: Test
test_gaps = TestCase $ do
  let m = Museq { _dur = 10,
                  _sup = 10,
                  _vec = V.fromList [ Event () (2,5) ()
                                    , Event () (6,8) () ] }
    in do
    assertBool "" $ [(0,2), (8,10)] == exteriorGaps m
    assertBool "" $ [(5,6)]         == interiorGaps m

  let m = Museq { _dur = 10,
                  _sup = 10,
                  _vec = V.fromList [ Event () (2,3) ()
                                    , Event () (4,5) ()
                                    , Event () (6,11) () ] }
    in do
    assertBool "" $ [(1,2)]        == exteriorGaps m
    assertBool "" $ [(3,4), (5,6)] == interiorGaps m

  let m = Museq { _dur = 10,
                  _sup = 10,
                  _vec = V.fromList [ Event () (2,7) ()
                                    , Event () (6,13) () ] }
    in do
    assertBool "" $ [] == exteriorGaps m
    assertBool "" $ [] == interiorGaps m

test_separateVoices :: Test
test_separateVoices = TestCase $ do
  let quads @ [a,b,c,d] = [ ( "a", RTime 0, RTime 1, "a1" )
                          , ( "b", RTime 0, RTime 2, "b1" )
                          , ( "a", RTime 2, RTime 3, "a2" )
                          , ( "b", RTime 4, RTime 6, "b2" ) ]
  assertBool "" $ separateVoices (mkMuseq 10 quads) ==
    M.fromList [ ( "a", [ Event "a" (0,1) "a1"
                        , Event "a" (2,3) "a2" ] )
               , ( "b", [ Event "b" (0,2) "b1"
                        , Event "b" (4,6) "b2" ] ) ]

test_timeToFinish :: Test
test_timeToFinish = TestCase $ do
  assertBool "1" $ 1 == timeToFinish mempty
  assertBool "1" $ 2 == timeToFinish
    (sup .~ 1 $ mmh 2 $ pre2 "" $ [ (0, "a") ] )
  assertBool "1" $ 2 == timeToFinish
    (dur .~ 1 $ mmh 2 $ pre2 "" $ [ (0, "a") ] )

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

testMuseqIsValid :: Test
testMuseqIsValid = TestCase $ do
  assertBool "valid, empty"               $ museqIsValid
    $ (mkMuseqFromEvs 3 [] :: Museq String ())
  assertBool "invalid, zero length" $ not $ museqIsValid
    $ (mkMuseqFromEvs 0 [] :: Museq String ())
  assertBool "valid, nonempty"            $ museqIsValid
    $ mkMuseqFromEvs 1 [mkEv0 "1" 0 () ]
  assertBool "invalid, time > _sup" $ not $ museqIsValid
    $ mkMuseqFromEvs 1 [mkEv0 "1" 2 () ]

testStack :: Test
testStack = TestCase $ do
  let a = mkMuseqH 1 [("a", RTime 0, "a")]
      b = mkMuseqH 1 [("a", RTime 0, "b")]
      c = mkMuseqH 1 [("a", RTime 0, "c")]
  assertBool "1" $ stack [a,b,c] ==
    Museq {_dur = 1, _sup = 1, _vec = V.fromList
            [ Event "a"   (0, 1) "a"
            , Event "aa"  (0, 1) "b"
            , Event "aaa" (0, 1) "c" ] }

  let y = mkMuseqFromEvs 2 [mkEv () 0 3 "()"]
      z = mkMuseqFromEvs 3 [mkEv "z" 1 2 "z"]
  assertBool "stack" $ stack2 y z ==
    (dur .~ (_dur z))
    ( mkMuseqFromEvs 6 [ mkEv "()"  0 3 "()"
                       , mkEv "az" 1 2 "z"
                       , mkEv "()"  2 5 "()"
                       , mkEv "az" 4 5 "z"
                       , mkEv "()"  4 7 "()"] )
  assertBool "stack" $ stack2 (dur .~ 1 $ y) z ==
    (dur .~ _dur z)
    ( mkMuseqFromEvs 6 [ mkEv "()"  0 3 "()"
                       , mkEv "az" 1 2 "z"
                       , mkEv "()"  2 5 "()"
                       , mkEv "az" 4 5 "z"
                       , mkEv "()"  4 7 "()" ] )
  assertBool "stack, where timeToAppearToFinish differs from timeToFinish"
    $ stack2 (sup .~ 1 $ y) z ==
    (dur .~ _dur z)
    ( sup .~ 6 $
      mkMuseqFromEvs 3 [ mkEv "()"  0 3 "()"
                       , mkEv "az"  1 2 "z"
                       , mkEv "()"  1 4 "()"
                       , mkEv "()"  2 5 "()"

    -- If I used timeToAppearToFinish instead of timeToFinish,
    -- this redundant second half would not be present.
                       , mkEv "()"  3 6 "()"
                       , mkEv "az"  4 5 "z"
                       , mkEv "()"  4 7 "()"
                       , mkEv "()"  5 8 "()" ] )

testRev :: Test
testRev = TestCase $ do
  let a = mkMuseqFromEvs 2 [ mkEv () 0     1     "a"
                           , mkEv () (1/3) 3     "b"
                           , mkEv () (1/2) (1/2) "c" ]
  assertBool "rev" $ rev a ==
    mkMuseqFromEvs 2 [ mkEv () 0     1               "a"
                     , mkEv () (3/2) (          3/2) "c"
                     , mkEv () (5/3) ((3-1/3) + 5/3) "b" ]

testEarlyAndLate :: Test
testEarlyAndLate = TestCase $ do
  let a = mkMuseqFromEvs 10 [ mkEv () 0 11 "a"
                            , mkEv () 1 2 "b"]
  assertBool "early" $ _vec (early 1 a) ==
    V.fromList [ mkEv () 0 1 "b"
               , mkEv () 9 20 "a"]

  let a' = mkMuseqFromEvs 10 [ mkEv () 0 11 "a"
                             , mkEv () 1 2 "b"]
  assertBool "late" $ _vec (late 1 a') ==
    V.fromList [ mkEv () 1 12 "a"
               , mkEv () 2 3 "b"]

testFastAndSlow :: Test
testFastAndSlow = TestCase $ do
  let a = mkMuseqFromEvs 10 [mkEv () 0 20 "a",mkEv () 2 2 "b"]
  assertBool "fast" $ (fast 2 a) ==
    mkMuseqFromEvs 5 [mkEv () 0 10 "a",mkEv () 1 1 "b"]
  assertBool "slow" $ (slow 2 a) ==
    mkMuseqFromEvs 20 [mkEv () 0 40 "a",mkEv () 4 4 "b"]

testDenseAndSparse :: Test
testDenseAndSparse = TestCase $ do
  let x = mkMuseqFromEvs 10 [mkEv () 0 15 "a",
                             mkEv () 2 2 "b"]
  assertBool "dense" $ dense 2 x ==
    (dur .~ 10 $ mkMuseqFromEvs 5 [ mkEv () 0 (15/2) "a"
                                  , mkEv () 1 1 "b"] )
  assertBool "sparse" $ sparse 2 x ==
    (dur .~ 10 $ mkMuseqFromEvs 20 [ mkEv () 0 30 "a"
                                   , mkEv () 4 4 "b"] )

testExplicitReps :: Test
testExplicitReps = TestCase $ do
  let y = Museq {_dur = 3, _sup = 4,
                 _vec = V.fromList [ ev4 "a" 0 3 ()
                                   , ev4 "b" 1 1 () ] }

  assertBool "unsafeExplicitReps" $ unsafeExplicitReps 24 y ==
    [ V.fromList [ ev4 "a" 0  3  ()
                 , ev4 "b" 1  1  () ]
    , V.fromList [ ev4 "a" 4  7  ()
                 , ev4 "b" 5  5  () ]
    , V.fromList [ ev4 "a" 8  11 () ]
    , V.fromList [ ev4 "b" 9  9  () ]
    , V.fromList [ ev4 "a" 12 15 ()
                 , ev4 "b" 13 13 () ]
    , V.fromList [ ev4 "a" 16 19 ()
                 , ev4 "b" 17 17 () ]
    , V.fromList [ ev4 "a" 20 23 () ]
    , V.fromList [ ev4 "b" 21 21 () ]
    ]

  assertBool "explicitReps" $ explicitReps y ==
    [ V.fromList [ ev4 "a" 0 3  ()
                 , ev4 "b" 1 1  () ]
    , V.fromList [ ev4 "a" 4 7  ()   -- Starts at time 3.
                 -- This rep only lasts until time = 6, but the end of the event that starts at 4 can be later than 6).
                 , ev4 "b" 5 5  () ]
    , V.fromList [ ev4 "a" 8 11 () ] -- Starts at 6 mod 4 = 2 in `y`.\
      -- The zero-duration event "b" starts at time 9.
      -- Repetitions are half-open (they do not include their endpoint),
      -- so the 3rd "b" is part of the 4th rep, not the 3rd.
    , V.fromList [ ev4 "b" 9 9  () ] -- Starts at 9 mod 4 = 1 in `y`.
    ]

testAppend :: Test
testAppend = TestCase $ do
    let a = mkMuseqFromEvs 1 [mkEv () 0 1 "a"]
        a2  = a {_sup = RTime $ 2}
        a12 = a {_sup = RTime $ 1%2}
        a32 = a {_sup = RTime $ 3%2}
        b = mkMuseqFromEvs 1 [mkEv () 0 0 "b"]
    assertBool "testAppend" $ append a b ==
      mkMuseqFromEvs 2 [mkEv () 0 1 "a", mkEv () 1 1 "b"]
    assertBool "testAppend" $ append a2 b ==
      let m = mkMuseqFromEvs 2 [mkEv () 0 1 "a"
                               ,mkEv () 1 1 "b"
                               ,mkEv () 3 3 "b"]
      in m {_sup = 4}
    assertBool "testAppend" $ append a12 b ==
      mkMuseqFromEvs 2 [mkEv () 0 1 "a"
                       ,mkEv () (1%2) (3%2) "a"
                       , mkEv () 1 1 "b"]
    assertBool "testAppend" $ append a32 b ==
      let m = mkMuseqFromEvs 2 [ mkEv () 0 1 "a"
                               , mkEv () 1 1 "b"
                               , mkEv () (2+1/2) (3+1/2) "a"
                               , mkEv () 3 3 "b", mkEv () 5 5 "b"]
      in m {_sup = 6}

testCat :: Test
testCat = TestCase $ do
  let a = mkMuseqH 1 [("a", RTime 0, "a")]
      b = mkMuseqH 1 [("b", RTime 0, "b")]
      c = mkMuseqH 1 [("c", RTime 0, "c")]
  assertBool "1" $ cat [a,b,c] ==
    mkMuseq 3  [ ("a", RTime 0, RTime 1, "a")
               , ("b", RTime 1, RTime 2, "b")
               , ("c", RTime 2, RTime 3, "c") ]
  assertBool "1" $ cat [a, dur .~ 2 $ b] ==
    mkMuseq 3  [ ("a", RTime 0, RTime 1, "a")
               , ("b", RTime 1, RTime 2, "b")
               , ("b", RTime 2, RTime 3, "b") ]

testRep :: Test
testRep = TestCase $ do
  let a = mkMuseqFromEvs 6 [mkEv () 0 7 "a"]
  assertBool "rep int" $ rep 2 a ==
    (dur .~ 12) (mkMuseqFromEvs 6 [mkEv () 0 7 "a"])
  assertBool "rep fraction" $ rep (3/2) a ==
    (dur .~ 9) (mkMuseqFromEvs 6 [mkEv () 0 7 "a"])

testMuseqsDiff :: Test
testMuseqsDiff = TestCase $ do
  let msg = M.singleton "amp" 1
      m3 = M.fromList [("a", mkMuseqFromEvs 10
                             [ mkEv0 "1" 0  (Note Boop msg)])
                      ,("b", mkMuseqFromEvs 15 [ mkEv0 "1" 0  (Note Boop msg)
                                               , mkEv0 "2" 10 (Note Boop msg)
                                               ] ) ]
      m2 = M.fromList [("a", mkMuseqFromEvs 10
                             [ mkEv0 "2" 0  (Note Vap msg) ])
                      ,("b", mkMuseqFromEvs 15
                             [ mkEv0 "2" 0  (Note Boop msg)
                             , mkEv0 "3" 10 (Note Boop msg)
                             ] ) ]
  assertBool "museqDiff" $ museqSynthsDiff m3 m2 == ( [ (Boop,"1") ]
                                               , [ (Boop,"3")
                                                 , (Vap ,"2")
                                                 ] )
  assertBool "museqDiff" $ museqSynthsDiff m2 m3 == ( [ (Boop,"3")
                                                 , (Vap ,"2") ]
                                               , [ (Boop,"1")
                                                 ] )

testArc :: Test
testArc = TestCase $ do
  let m = mkMuseqFromEvs 5 [ Event () (0,6) "a"
                           , Event () (2,4) "b"]
  -- arguments to arc : time0 tempoPeriod from to museq
  assertBool "arc 0" $ arc 100 2 200 210  m
    == [ ( Event () (200,202) "a" ) -- this "a" started at 190
       , ( Event () (200,210) "a" ) -- this "a" will end at 212
       , ( Event () (204,208) "b" )]
  assertBool "arc 1" $ arc 101 2 200 210  m
    == [ ( Event () (200,203) "a") -- started at 191
       , ( Event () (201,210) "a") -- ends at 213
       , ( Event () (205,209) "b")]
  assertBool "arc 1" $ arc 101 2 200 220  m
    == [ ( Event () (200,203) "a")
       , ( Event () (201,213) "a")
       , ( Event () (205,209) "b")
       , ( Event () (211,220) "a")
       , ( Event () (215,219) "b")]

testOverParams :: Test
testOverParams = TestCase $ do
  let m = mkMuseqFromEvs 2 [ mkEv0 () 0 $ M.singleton "freq" 100
                           , mkEv0 () 1 $ M.singleton "amp"  0.1 ]
  assertBool "overParams" $ overParams [("freq",(+1))] m
    == mkMuseqFromEvs 2 [ mkEv0 () 0 $ M.singleton "freq" 101
                        , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "switchParams" $ switchParams [("freq","guzzle")] m
    == mkMuseqFromEvs 2 [ mkEv0 () 0 $ M.singleton "guzzle" 100
                        , mkEv0 () 1 $ M.singleton "amp" 0.1]
  assertBool "keepParams" $ keepParams ["freq"] m
    == mkMuseqFromEvs 2 [mkEv0 () 0 $ M.singleton "freq" 100]
  assertBool "dropParams" $ dropParams ["freq"] m
    == mkMuseqFromEvs 2 [mkEv0 () 1 $ M.singleton "amp" 0.1]

testBoundaries :: Test
testBoundaries = TestCase $ do
  assertBool "boundaries" $ boundaries [(0,1),(1,1),(2,3::Int)]
    == [0,1,1,2,3]
  assertBool "boundaries" $
    boundaries [(0,2), (1,10), (3,4), (3,5), (4,4)]
    == [0,1,2,3,4,4,5,10]

testPartitionArcAtTimes :: Test
testPartitionArcAtTimes = TestCase $ do
  assertBool "partitionArcAtTimes" $
    partitionArcAtTimes [0,2,2,5,10::Int] (0,5)
    == [(0,2),(2,2),(2,5)]

testPartitionAndGroupEventsAtBoundaries :: Test
testPartitionAndGroupEventsAtBoundaries = TestCase $ do
  let a   = ev4 () 0 1 'a'
      b   = ev4 () 1 2 'b'
      c   = ev4 () 0 2 'c'
      c01 = ev4 () 0 1 'c'
      c12 = ev4 () 1 2 'c'
      es = [a,b,c]
  assertBool "partitionAndGroupEventsAtBoundaries" $
    partitionAndGroupEventsAtBoundaries
    (boundaries $ map _evArc es) es
    == [a,c01,b,c12]

testMerge :: Test
testMerge = TestCase $ do
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
  assertBool "merge0a" $ merge0a a' bc'
    == Museq { _dur = 3, _sup = 6,
               _vec = V.fromList [ mkEv "ab" 0 1 $ M.fromList [("amp",2)
                                                              ,("freq",2)]
                                 , mkEv "ac" 4 5 $ M.fromList [("amp",2)
                                                              ,("freq",0)] ] }

testMeta :: Test
testMeta = TestCase $ do
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

testMuseqNamesAreValid :: Test
testMuseqNamesAreValid = TestCase $ do
  assertBool "empty Museq has valid names" $ museqMaybeNamesAreValid $
    mkMuseqFromEvs 10 ([] :: [Ev (Maybe String) ()])
  assertBool "Museq without names has valid names"
    $ museqMaybeNamesAreValid
    $ mkMuseqFromEvs 10 [ mkEv (Nothing :: Maybe String) 0 10 ()
                        , mkEv (Nothing :: Maybe String) 0 10 () ]
  assertBool "Museq with overlapping like names is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseqFromEvs 10 [ mkEv (Just "1") 0  6 ()
                                                , mkEv (Just "1") 4 10 () ]
  assertBool "Museq with non-overlapping like names is valid" $
    museqMaybeNamesAreValid $ mkMuseqFromEvs 10 [ mkEv (Just "1") 0 4  ()
                                                , mkEv (Just "1") 6 10 () ]
  assertBool "Museq with overlapping unlike names is valid" $
    museqMaybeNamesAreValid $ mkMuseqFromEvs 10 [ mkEv (Just "1") 0 6 ()
                                                , mkEv (Just "2") 4 10 () ]
  assertBool "Museq with wrapped overlap is not valid" $ not $
    museqMaybeNamesAreValid $ mkMuseqFromEvs 10 [ mkEv (Just "1") 0 4 ()
                                                , mkEv (Just "1") 6 12 () ]

testIntNameEvents :: Test
testIntNameEvents = TestCase $ do
  assertBool "intNameEvents, no overlap" $
    intNameEvents 10 [mkEv () 0  1 (),  mkEv () 2 3 ()]
    ==               [mkEv  1 0  1 (),  mkEv  1 2 3 ()]
  assertBool "intNameEvents, ordinary overlap" $
    intNameEvents 10 [mkEv () 0 2 (),  mkEv () 1 3 ()]
    ==               [mkEv  1 0 2 (),  mkEv  2 1 3 ()]
  assertBool "intNameEvents, wrapped overlap" $
    intNameEvents 10 [mkEv () 0 2 (),  mkEv () 5 11 ()]
    ==               [mkEv  1 0 2 (),  mkEv  2 5 11 ()]

testNameAnonEvents :: Test
testNameAnonEvents = TestCase $ do
  let m = mkMuseqFromEvs 10 [ mkEv (Just "1") 0 1 ()
                            , mkEv Nothing    0 1 () ]
  assertBool "testNameAnonEvents" $ nameAnonEvents m
    ==    mkMuseqFromEvs 10 [ mkEv "1"  0 1 ()
                            , mkEv "a1" 0 1 () ]
