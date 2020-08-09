{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Montevideo.Monome.Test.Windows where

import Test.HUnit

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import qualified Data.Set as S

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Test.Data
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Keyboard as K
import           Montevideo.Monome.Window.Shift    as Sh
import           Montevideo.Synth
import           Montevideo.Monome.Window.Util
import           Montevideo.Util


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  , TestLabel "test_keyboardHandler" test_keyboardHandler
  , TestLabel "test_edoKey_ScAction" test_edoKey_ScAction
  ]

test_edoKey_ScAction :: Test
test_edoKey_ScAction = TestCase $ do
  let sustainedVoice = VoiceId 0
      sustainedXy = (0,0)
      newVoice = VoiceId 1
      newXy = (0,1)
      st = st0 & ( stApp . edoSustaineded .~
                   Just (S.singleton sustainedVoice) )
      newPitch = xyToEdo_app (st ^. stApp) newXy
  assertBool "pressing a key that's sustained has no effect" $
    edoKey_ScAction (st ^. stApp) sustainedVoice (sustainedXy, True)
    == []
  assertBool "releasing a key that's sustained has no effect" $
    edoKey_ScAction (st ^. stApp) sustainedVoice (sustainedXy, False)
    == []

  assertBool "press a key that's not sustained.\n" $
    edoKey_ScAction (st ^. stApp) newVoice (newXy, True) ==
    [ ScAction_New
      { _actionSynthDefEnum = Moop
      , _actionSynthName = newVoice
      , _actionScMsg = M.fromList
        [ ("freq", Config.freq *
                   edoToFreq (st ^. stApp . edoConfig) newPitch)
        , ( "amp", Config.amp ) ] } ]

  assertBool "release a key that's not sustained" $
    edoKey_ScAction (st ^. stApp) newVoice (newXy, False) ==
    [ ScAction_Free
      { _actionSynthDefEnum = Moop
      , _actionSynthName = newVoice } ]

test_shiftHandler :: Test
test_shiftHandler = TestCase $ do
  assertBool "releasing a shift button does nothing" $
    fromRight (error "bork")
    (Sh.handler st_0a (meh, False))
    =^= st_0a

  assertBool "shift the notes one space closer to player's body" $ let
    oldShift = st_0a ^. stApp . edoXyShift
    newShift = pairAdd oldShift $
               fromRight (error "bork") $
               Sh.shift (st_0a ^. stApp . edoConfig) Sh.downArrow
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys (st_0a ^. stApp . edoConfig) oldShift pc0)
      ++ map (,True)  (pcToXys (st_0a ^. stApp . edoConfig) newShift pc0)
    in fromRight (error "bork")
       (Sh.handler st_0a (Sh.downArrow, True))
       =^= (st_0a & stPending_Monome .~ msgs
                  & stApp . edoXyShift .~ newShift)

  assertBool "shift the notes an octave higher" $ let
    oldShift = st_0a ^. stApp . edoXyShift
    newShift = pairAdd oldShift $
               fromRight (error "bork") $
               Sh.shift (st_0a ^. stApp . edoConfig) Sh.upOctave
    msgs :: [LedMsg] = map (K.label,)
      $  map (,False) (pcToXys (st_0a ^. stApp . edoConfig) oldShift pc0)
      ++ map (,True)  (pcToXys (st_0a ^. stApp . edoConfig) newShift pc0)
    in fromRight (error "bork")
       (Sh.handler st_0a (Sh.upOctave, True)) =^=
       (st_0a & stPending_Monome .~ msgs
              & stApp . edoXyShift .~ newShift)

test_keyboardHandler :: Test
test_keyboardHandler = TestCase $ do
  assertBool "Releasing a key sends off-messages to monome, sends off-messages to Vivid, removes something from _edoFingers, and removes some things from _edoLit." $
    fromRight (error "bork")
    (K.handler st_01f (xy1, False))
    =^= ( st_0f
          & ( stPending_Monome .~
              map (\xy -> (K.label, (xy, False)) )
              (pcToXys_st st_01f pitch1 ) )
          & ( stPending_Vivid .~
              [ ScAction_Free
                { _actionSynthDefEnum = Moop
                , _actionSynthName = v1 } ] ) )

  assertBool "releasing a key that's also the anchor pitch sends no monome messages" $
    fromRight (error "bork")
    (K.handler st_0af (xy0, False))
    =^= ( st_0af
          & ( stApp . edoLit . at pc0 . _Just
              .~ S.singleton LedBecauseAnchor )
          & stApp . edoFingers .~ mempty
          & stPending_Vivid .~
          [ ScAction_Free
            { _actionSynthDefEnum = Moop
            , _actionSynthName = v0 } ] )

  assertBool "releasing a key that's a sustained voice sends no vivid or monome messages, but updates lit and fingers" $
    fromRight (error "bork")
    (K.handler st_0fs (xy0, False))
    =^= ( st_0fs
          & ( stApp . edoLit . at pc0 . _Just
              .~ S.singleton LedBecauseSustain )
          & stApp . edoFingers .~ mempty )

  assertBool "pressing a key that's a pitch from a sustained voice does everything it would do if that weren't the case." $
    fromRight (error "bork")
    (K.handler st_0s (xy0, True))
    =^= ( let nv = (nextVoice $ _stVoices st_0s)
          in st_0s
             & ( stApp . edoLit . at pc0 . _Just
                 %~ S.insert (LedBecauseSwitch xy0) )
             & ( stVoices %~ M.insert
                 (nextVoice $ _stVoices st_0s)
                 ( Voice { _voiceSynth = Nothing
                         , _voicePitch = xyToEdo_app (_stApp st_0s) xy0
                         , _voiceParams = mempty } ) )
             & ( stPending_Vivid .~ edoKey_ScAction
                 (st0 ^. stApp) nv (xy0, True) )
             & ( stApp . edoFingers .~ M.fromList
                 [ (xy0,nv) ] ) )

  assertBool "pressing a key adds a voice to _stVoices, sends on-messages to monome, sends on-messages to Vivid, adds something to _edoFingers, and adds something from _edoLit" $
    fromRight (error "bork")
    (K.handler st_0f (xy1, True))
    -- PITFALL: st_01f != st_0f
    =^= ( let nv = nextVoice $ _stVoices st_0f
          in st_01f
             & ( stVoices %~ M.insert nv
                 (Voice { _voiceSynth = Nothing
                        , _voicePitch = xyToEdo_app (_stApp st_0f) xy1
                        , _voiceParams = mempty } ) )
             & ( stPending_Monome .~
                 ( map (\xy -> (K.label, (xy, True)) ) $
                   pcToXys_st st_01f pitch1 ) )
             & ( stPending_Vivid .~ edoKey_ScAction
                 (st0 ^. stApp)
                 (nextVoice $ _stVoices st_01f) (xy1, True) )
             & stApp . edoFingers %~ M.insert xy1 nv )
