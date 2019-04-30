{-# LANGUAGE ScopedTypeVariables #-}

module Vivid.Test.THode where

import           Data.Either
import qualified Data.Map as M
import           Test.HUnit

import Hode.Hode hiding (Test)
import Vivid.Dispatch.Abbrevs
import Vivid.Hode
import Util


testRslt :: Rslt
testRslt = mkRslt $ M.fromList $ _baseRslt ++
  [ (01,Phrase' "a")
  , (02,Phrase' "0")
  , (03,Phrase' "400")
  , (06,Phrase' "1")
  , (04, Rel' $ Rel [3] (aFreq))
  , (05, Rel' $ Rel [1,2,4] (aWhenPlays))
  , (07,Phrase' "500")
  , (08, Rel' $ Rel [7] (aFreq))
  , (09, Rel' $ Rel [1,6,8] (aWhenPlays))
  , (10,Phrase' "3")
  , (11, Rel' $ Rel [10,1] (aMmho))
  , (12, Rel' $ Rel [11] (aNBoop))
  , (13, Rel' $ Rel [12] (aPlaying))]

test_module_hode :: Test
test_module_hode = TestList [
    TestLabel "testEvalSynthParam" testEvalSynthParam
  , TestLabel "testEvalParamEvent" testEvalParamEvent
  , TestLabel "testEvalEventTriples" testEvalEventTriples
  , TestLabel "testEvalMmho" testEvalMmho
  ]

testEvalMmho :: Test
testEvalMmho = TestCase $ do
  assertBool "1" $ evalMmho testRslt 11
    == Right ( mmho 3 $ pre2 "a"
               [ (0, m1 "freq" 400)
               , (1, m1 "freq" 500) ] )

testEvalSynthParam :: Test
testEvalSynthParam = TestCase $ do
  let r :: Rslt
      r = mkRslt $ M.fromList $ _baseRslt ++
        [(6,Phrase' "500")
        ,(7,Rel' (Rel [6] aFreq))]
  assertBool "1" $ evalSynthParam r 7
    == Right (M.singleton "freq" 500)
  assertBool "2" $ isLeft $ evalSynthParam r 6

testEvalParamEvent :: Test
testEvalParamEvent = TestCase $ do
  let r :: Rslt
      r = mkRslt $ M.fromList $ _baseRslt ++
          [ (1,Phrase' "a")
          , (2,Phrase' "0")
          , (3,Phrase' "400")
          , (4,Rel' (Rel [3] aFreq))
          , (5,Rel' (Rel [1,2,4] aWhenPlays)) ]
  assertBool "1" $ evalParamEvent r 5
    == Right ("a",0,M.singleton "freq" 400)

testEvalEventTriples :: Test
testEvalEventTriples = TestCase $ do
  let r :: Rslt = mkRslt $ M.fromList $ _baseRslt ++
                  [ (1,Phrase' "a")
                  , (2,Phrase' "0")
                  , (3,Phrase' "400")
                  , (4,Rel' $ Rel [3] aFreq)
                  , (5,Rel' $ Rel [1,2,4] aWhenPlays)
                  , (6,Phrase' "1")
                  , (7,Phrase' "500")
                  , (8,Rel' $ Rel [7] aFreq)
                  , (9,Rel' $ Rel [1,6,8] aWhenPlays) ]
  assertBool "1" $ evalEventTriples r 1
    == Right [("a", 0.0, M.singleton "freq" 400)
             ,("a", 1.0, M.singleton "freq" 500)]
