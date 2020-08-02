{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections
, TypeApplications
, ScopedTypeVariables #-}

module Montevideo.Monome.Test.Misc where

import Control.Lens
import Data.Map as M
import Data.Set as S
import Test.HUnit

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Test.Data
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import           Montevideo.Monome.Window.Util
import           Montevideo.Synth


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
      st = st0 & ( stApp . edoSustaineded .~
                   Just (S.singleton sustainedVoice) )
      newPitch = xyToEdo_app (st ^. stApp) newVoice
  assertBool "pressing a key that's sustained has no effect" $
    etKey_SoundMsg (st ^. stApp) (sustainedVoice, True) == []
  assertBool "releasing a key that's sustained has no effect" $
    etKey_SoundMsg (st ^. stApp) (sustainedVoice, False) == []

  assertBool "press a key that's not sustained.\n" $
    etKey_SoundMsg (st ^. stApp) (newVoice, True) ==
    [ SoundMsg { _soundMsgVoiceId = newVoice
               , _soundMsg_ScAction = ScAction_Send
                 { _actionSynthDefEnum = Boop
                 , _actionSynthName = "todo -- use this and not voiceId"
                 , _actionScMsg = M.fromList
                   [ ("freq", Config.freq *
                              edoToFreq (st ^. stApp . edoConfig) newPitch)
                   , ( "amp", Config.amp ) ] } } ]

  assertBool "release a key that's not sustained" $
    etKey_SoundMsg (st ^. stApp) (newVoice, False) ==
    [ SoundMsg { _soundMsgVoiceId = newVoice
               , _soundMsg_ScAction = ScAction_Send
                     { _actionSynthDefEnum = Boop
                     , _actionSynthName = "todo -- use this and not voiceId"
                     , _actionScMsg = M.singleton "amp" 0 } } ]

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
  let w1 = Window "w1" (\(x,y) -> x > y) id $ \st _ -> Right st
      w2 = Window "w2" (\(x,_) -> x > 4) id $ \st _ -> Right st
      w3 = Window "w3" (\(_,_) -> True)  id $ \st _ -> Right st
      ws = [w1,w2,w3]
  assertBool "caught by w1 before reaching w3" $
    not $ belongsHere ws w3 (1,0)
  assertBool "caught by w2 before reaching w3" $
    not $ belongsHere ws w3 (5,6)
  assertBool "should reach w3, which contains it" $
    belongsHere ws w3 (0,0)
  assertBool "should reach w2, but w2 does not contain it" $
    not $ belongsHere ws w2 (1,2)
