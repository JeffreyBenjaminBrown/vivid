module Vivid.Hode.HTest where

import           Data.Map (Map)
import qualified Data.Map as M
import           Test.HUnit

import Hode.Hode hiding (Test)
import Vivid.Hode


testRslt :: Rslt
testRslt = mkRslt $ M.fromList $ _baseRslt ++ 
  [(1,Phrase' "400")
  ,(2,Rel' (Rel [1] (-5)))
  ,(3,Phrase' "a")
  ,(4,Phrase' "0")
  ,(5,Rel' (Rel [2,3,4] (-3)))
  ,(6,Phrase' "500")
  ,(7,Rel' (Rel [6] (-5)))
  ,(8,Phrase' "1")
  ,(9,Rel' (Rel [7,3,8] (-3)))
  ,(10,Phrase' "3")
  ,(11,Rel' (Rel [3] (-13)))
  ,(12,Rel' (Rel [10,11] (-11)))
  ,(13,Rel' (Rel [12] (-9)))
  ,(14,Rel' (Rel [13] (-7)))]

test_module_hode :: Test
test_module_hode = TestList [
  TestLabel "testEvalSynthParam" testEvalSynthParam
  ]

testEvalSynthParam :: Test
testEvalSynthParam = TestCase $ do
  assertBool "1" $ evalSynthParam testRslt 7
    == Right (M.singleton "freq" 500)
