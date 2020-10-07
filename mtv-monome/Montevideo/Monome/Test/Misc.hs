{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TypeApplications #-}

module Montevideo.Monome.Test.Misc where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.HUnit

import Montevideo.Monome.Test.Data
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Util
import Montevideo.Monome.Window.Util


tests :: Test
tests = TestList [
    TestLabel "test_visible" test_visible
  , TestLabel "test_dependentPitchClass" test_dependentPitchClass
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

test_dependentPitchClass :: Test
test_dependentPitchClass = TestCase $ do
  let m :: LitPitches EdoApp
      m = M.singleton 10 $ S.singleton $ LedBecauseSwitch (1,1)
  assertBool "ledBecause_toPitchClass finds it" $
    ledBecause_toPitchClass @ EdoApp m (LedBecauseSwitch (1,1)) == Just 10
  assertBool "ledBecause_toPitchClass does not find it" $
    ledBecause_toPitchClass @ EdoApp m (LedBecauseSwitch (1,0)) == Nothing

test_visible :: Test
test_visible = TestCase $ do
  -- PITFALL: These labels (e.g. KeyboardWindow) don't really mean anything;
  -- I'm only using them to keep the labels distinct.
  let w1 = Window
           { windowLabel = KeyboardWindow
           , windowContains = (\(x,y) -> x > y)
           }
      w2 = Window
           { windowLabel = ShiftWindow
           , windowContains = (\(x,_) -> x > 4)
           }
      w3 = Window
           { windowLabel = SustainWindow
           , windowContains = \(_,_) -> True
           }

  let orig = (0,0) -- Every window is anchored at the origin, (0,0).
      ws = map (orig,) [w1,w2,w3]
    in do
    assertBool "caught by w1 before reaching w3" $
      not $ visible ws (orig, w3) (1,0)
    assertBool "caught by w2 before reaching w3" $
      not $ visible ws (orig, w3) (5,6)
    assertBool "should reach w3, which contains it" $
            visible ws (orig, w3) (0,0)
    assertBool "should reach w2, but w2 does not contain it" $
      not $ visible ws (orig, w2) (1,2)

  -- For these tests the top left corners of the windows differ.
  assertBool "Since 5 > 0, if w1 were anchored at (0,0) it would obscure xy = (5,0) (both relatively and absolutely) in w2. But w1 is anchored at (10,0), and xy relative to that anchor is at (-5,0), so w1 should not block it." $ let
    ws = [ ((10,0), w1)
         , ((0,0), w2) ]
    in visible ws ((0,0),w2) (5,0)
  assertBool "Since 1 < 3, if w1 were anchored at (0,0), it would not obscure xy = (1,3) (both relatively and absolutely) in w2. But w1 is anchored at (0,3), and xy relative to that anchor is at (1,0), and since 1 > 0, w1 should block it from reaching w2." $ let
    ws = [ ((0,3), w1)
         , ((0,0), w2) ]
    in not $ visible ws ((0,0),w2) (1,3)

