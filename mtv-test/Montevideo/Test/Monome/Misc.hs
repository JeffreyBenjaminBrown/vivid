{-# LANGUAGE TupleSections
, TypeApplications
, ScopedTypeVariables #-}

module Montevideo.Test.Monome.Misc where

import Control.Lens
import Data.Map as M
import Data.Set as S
import Test.HUnit

import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.Math31
import           Montevideo.Test.Monome.Data
import           Montevideo.Monome.Types.Button
import           Montevideo.Monome.Types.Initial
import           Montevideo.Monome.Types.Window
import           Montevideo.Monome.Window.Common


tests :: Test
tests = TestList [
    TestLabel "testBelongsHere" testBelongsHere
  , TestLabel "testDependentPitchClass" testDependentPitchClass
  , TestLabel "test_etKey_SoundMsg" test_etKey_SoundMsg
  ]

test_etKey_SoundMsg :: Test
test_etKey_SoundMsg = TestCase $ do
  let sustainedVoice :: VoiceId = (0,0)
      newVoice :: VoiceId = (0,1)
      st = st0 & ( stApp . etSustaineded .~
                   Just (S.singleton sustainedVoice) )
      newPitch = xyToEt31_st st newVoice
  assertBool "pressing a key that's sustained has no effect" $
    etKey_SoundMsg st (sustainedVoice, True) == []
  assertBool "releasing a key that's sustained has no effect" $
    etKey_SoundMsg st (sustainedVoice, False) == []
  assertBool "press a key that's not sustained" $
    etKey_SoundMsg st (newVoice, True) ==
    [ SoundMsg { _soundMsgVoiceId = newVoice
               , _soundMsgPitch = Just newPitch
               , _soundMsgVal = 100 * et31ToFreq newPitch
               , _soundMsgParam = "freq" }
    , SoundMsg { _soundMsgVoiceId = newVoice
               , _soundMsgPitch = Just newPitch
               , _soundMsgVal = Config.amp
               , _soundMsgParam = "amp" } ]
  assertBool "release a key that's not sustained" $
    etKey_SoundMsg st (newVoice, False) ==
    [ SoundMsg { _soundMsgVoiceId = newVoice
               , _soundMsgPitch = Nothing
               , _soundMsgVal = 0
               , _soundMsgParam = "amp" } ]

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
  let w1 = Window "w1" (\(x,y) -> x > y) id const
      w2 = Window "w2" (\(x,_) -> x > 4) id const
      w3 = Window "w3" (\(_,_) -> True)  id const
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 (1,0)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 (5,6)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 (0,0)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 (1,2)
