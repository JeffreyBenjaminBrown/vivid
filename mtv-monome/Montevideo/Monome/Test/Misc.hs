{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TypeApplications #-}

module Montevideo.Monome.Test.Misc where

import Data.Map as M
import Data.Set as S
import Test.HUnit

import Montevideo.Monome.Test.Data
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Window.Common
import Montevideo.Monome.Window.Util


tests :: Test
tests = TestList [
    TestLabel "testBelongsHere" testBelongsHere
  , TestLabel "testDependentPitchClass" testDependentPitchClass
  , TestLabel "test_nextVoice" test_nextVoice
  ]

test_nextVoice :: Test
test_nextVoice = TestCase $ do
  let m = M.fromList [ (VoiceId 0, error "meh")
                     , (VoiceId 2, error "meh") ]
  assertBool "next voice in mempty is 0" $
    nextVoice ( st0 {_stVoices = mempty} ) == VoiceId 0
  assertBool "next voice in m is 3" $
    nextVoice ( st0 {_stVoices = m} ) == VoiceId 3

testDependentPitchClass :: Test
testDependentPitchClass = TestCase $ do
  let m :: LitPitches EdoApp
      m = M.singleton 10 $ S.singleton $ LedBecauseSwitch (1,1)
  assertBool "ledBecause_toPitchClass finds it" $
    ledBecause_toPitchClass @ EdoApp m (LedBecauseSwitch (1,1)) == Just 10
  assertBool "ledBecause_toPitchClass does not find it" $
    ledBecause_toPitchClass @ EdoApp m (LedBecauseSwitch (1,0)) == Nothing

testBelongsHere :: Test
testBelongsHere = TestCase $ do
  -- PITFALL: These labels (e.g. KeyboardWindow) don't really mean anything;
  -- I'm only using them to keep the labels distinct.
  let w1 = Window
           { windowLabel = KeyboardWindow
           , windowContains = (\(x,y) -> x > y)
           , windowInitLeds = const $ const []
           , windowHandler = \st _ -> Right st
           }
      w2 = Window
           { windowLabel = ShiftWindow
           , windowContains = (\(x,_) -> x > 4)
           , windowInitLeds = const $ const []
           , windowHandler = \st _ -> Right st
           }
      w3 = Window
           { windowLabel = SustainWindow
           , windowContains = \(_,_) -> True
           , windowInitLeds = const $ const []
           , windowHandler = \st _ -> Right st
           }
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 (1,0)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 (5,6)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 (0,0)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 (1,2)
