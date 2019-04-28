{-# LANGUAGE ScopedTypeVariables #-}

module Vivid.Hode.HTest where

import           Data.Either
import           Data.Map (Map)
import qualified Data.Map as M
import           Test.HUnit

import Hode.Hode hiding (Test)
import Vivid.Hode


testRslt :: Rslt
testRslt = mkRslt $ M.fromList $ _baseRslt ++
  [ (1,Phrase' "a")
  ,(2,Phrase' "0")
  ,(3,Phrase' "400")
  ,(4,Rel' (Rel [3] (-5)))
  ,(5,Rel' (Rel [1,2,4] (-3)))
  ,(6,Phrase' "1")
  ,(7,Phrase' "500")
  ,(8,Rel' (Rel [7] (-5)))
  ,(9,Rel' (Rel [1,6,8] (-3)))
  ,(10,Phrase' "3")
  ,(11,Rel' (Rel [1] (-13)))
  ,(12,Rel' (Rel [10,11] (-11)))
  ,(13,Rel' (Rel [12] (-9)))
  ,(14,Rel' (Rel [13] (-7)))]

test_module_hode :: Test
test_module_hode = TestList [
  TestLabel "testEvalSynthParam" testEvalSynthParam
  , TestLabel "testEvalParamEvent" testEvalParamEvent
  ]

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
