module Montevideo.Monome.Test.Sustain where

import Test.HUnit

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import Montevideo.Dispatch.Types.Many
import Montevideo.Monome.EdoMath
import Montevideo.Monome.Test.Data
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Window.Keyboard as K
import Montevideo.Monome.Window.Sustain  as Su
import Montevideo.Synth


tests :: Test
tests = TestList [
    TestLabel "test_sustainHandler" test_sustainHandler
  , TestLabel "test_deleteOneSustainReason_and_insertOneSustainReason"
    test_deleteOneSustainReason_and_insertOneSustainReason
  , TestLabel "test_sustainOn" test_sustainOn
  , TestLabel "test_sustainOff" test_sustainOff
  , TestLabel "test_sustained_minus_fingered" test_sustained_minus_fingered
  , TestLabel "test_sustainedVoices_inPitchClasses" test_sustainedVoices_inPitchClasses
  , TestLabel "test_sustainLess" test_sustainLess
  ]

test_sustainLess :: Test
test_sustainLess = TestCase $ do

  -- In the following, the fingered key is sustained when `sustainLess`
  -- is called. It continues to sound (hence vs is empty),
  -- but it is removed from the sustained pitches.
  let Right (st, vs) = sustainLess $ st0
        & stApp . edoFingers .~ M.fromList [ ((Monome_256,xy0), v0) ]
        & stApp . edoSustaineded .~ S.singleton v0
        & stApp . edoLit .~ ( M.singleton pc0
                              $ S.fromList [ LedBecauseSwitch xy0
                                           , LedBecauseSustain ] )
    in do
    assertBool "" $ st =^=
      ( st0
        & stApp . edoFingers .~ M.fromList [ ((Monome_256, xy0), v0) ]
        & stApp . edoSustaineded .~ mempty
        & stApp . edoLit .~ ( M.singleton pc0 $ S.singleton $
                              LedBecauseSwitch xy0 ) )
    assertBool "" $ vs == []

  let v = Voice { _voiceSynth = Nothing
                , _voicePitch = error "set below"
                , _voiceParams = mempty }
      st1 :: St EdoApp = st0
        & stApp . edoFingers .~ M.singleton (Monome_256,(0,0)) (VoiceId 0)
        & stApp . edoSustaineded .~ S.singleton (VoiceId 1)
        & ( stVoices .~ M.fromList
            [ ( VoiceId 0, v { _voicePitch = 0 } )
            , ( VoiceId 1, v { _voicePitch = EdoPitch $ st0 ^.
                                             stApp . edoConfig . edo } ) ] )
      Right (st2, vs) = sustainLess st1
    in do
    assertBool "" $ st2 == ( st1 & stApp . edoSustaineded .~ mempty )
    assertBool "" $ vs == [ VoiceId 1 ]

  let st = st0
        & stApp . edoFingers .~ M.fromList [ ((Monome_256, xy1), v1) ]
        & stApp . edoSustaineded .~ S.singleton v0
        & stApp . edoLit .~ ( M.singleton pc0
                              $ S.fromList [ LedBecauseSwitch xy0
                                           , LedBecauseSustain ] )
      Right (st', vs) = sustainLess st
      in do assertBool "" $ st =^= st'
            assertBool "" $ vs == []

test_sustainedVoices_inPitchClasses :: Test
test_sustainedVoices_inPitchClasses = TestCase $ do
  -- Pitch 1 is sustained and (in a different voice) fingered.
  -- Calling with pitch 1 or 1+e, return the sustained voice only.
  --
  assertBool "" $
    sustainedVoices_inPitchClasses st_0fs [pc0]
    == Right [v0]
  assertBool "" $
    sustainedVoices_inPitchClasses st_0fs [pc0, pc1]
    == Right [v0]
  assertBool "" $
    sustainedVoices_inPitchClasses st_0fs [pc1]
    == Right []

test_sustained_minus_fingered :: Test
test_sustained_minus_fingered = TestCase $ do
  assertBool "Turn off sustain. Voice 0 is fingered, 1 is turned off." $
    sustained_minus_fingered st_0fs_1s == S.singleton v1

test_sustainOn :: Test
test_sustainOn = TestCase $ do
  assertBool "turn sustain on" $
    fromRight (error "bork") (sustainMore st_0f) =^=
    ( st_0f & ( stApp . edoLit . at pc0 . _Just
                %~ S.insert LedBecauseSustain )
            & stApp . edoSustaineded .~ S.singleton v0 )
  assertBool "sustain won't turn on if nothing is fingered" $
    fromRight (error "bork") (sustainMore st0) =^= st0

test_sustainOff :: Test
test_sustainOff = TestCase $ do
  assertBool "turn sustain off" $
    fromRight (error "bork") (sustainOff st_0s)
    =^= ( st_0s
          & stApp . edoLit .~ mempty
          & stApp . edoSustaineded .~ mempty )

  assertBool "turn sustain off, but finger persists" $
    fromRight (error "bork") (sustainOff st_0fs)
    =^= ( st_0fs & ( stApp . edoLit . at pc0 . _Just
                     %~ S.delete LedBecauseSustain )
          & stApp . edoSustaineded .~ mempty )

test_deleteOneSustainReason_and_insertOneSustainReason :: Test
test_deleteOneSustainReason_and_insertOneSustainReason = TestCase $ do
  let
    pc = 0
    lit_a :: Map (EdoPitchClass) (Set LedBecause) = -- lit b/c anchor
      M.singleton pc $ S.singleton LedBecauseAnchor
    lit_s :: Map (EdoPitchClass) (Set LedBecause) = -- lit b/c/ sustain
      M.singleton pc $ S.singleton LedBecauseSustain
    lit_as :: Map (EdoPitchClass) (Set LedBecause) = -- lit b/c both
      M.singleton pc $ S.fromList [LedBecauseAnchor, LedBecauseSustain]
  assertBool "add sustain to the reasons a lit (because anchored) key is lit"
    $ insertOneSustainReason pc lit_a == lit_as
  assertBool "add sustain to the previously empty set of reasons a key is lit"
    $ insertOneSustainReason pc mempty == lit_s

  assertBool "if sustain was the only reason, then upon releasing sustain, there are no more reasons" $
    deleteOneSustainReason pc lit_s == mempty
  assertBool "if the anchor note is sustained, then upon releasing sustain, the anchor reemains as reason to light the key" $
    deleteOneSustainReason pc lit_as == lit_a

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ fromRight (error "bork")
    (Su.handler st0 (Monome_256, (meh , False)))
    =^= st0

  assertBool "THE TEST: turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome." $
    fromRight (error "bork")
    (Su.handler st_0f (Monome_256, (Su.button_sustainMore, True)))
    =^= ( st_0f & stApp . edoSustaineded .~ S.singleton v0
          & ( stApp . edoLit .~ M.singleton pc0
              ( S.fromList [ LedBecauseSustain
                           , LedBecauseSwitch xy0 ] ) )
          & stPending_Monome .~ buttonMsgs Monome_256 True )

  assertBool ( "turning sustain OFF does all this stuff:\n" ++
               "flip the sustain state\n" ++
               "emptiy the set of sustained voices\n" ++
               "remove all `LedBecauseSustain`s from reasons for lit keys\n"
               ++ "adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered\n" ++
               " adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered\n" ++
               "Pitch 0 is fingered, and 0 and 1 sounding; 1 turns off.") $

    fromRight (error "bork")
    (Su.handler st_0fs_1s (Monome_256, (Su.button_sustainOff, True)))
    =^= ( st_0fs_1s
          & stApp . edoSustaineded .~ mempty
          & stApp . edoLit .~ M.singleton pc0 ( S.singleton $
                                               LedBecauseSwitch xy0 )
          & stPending_Monome .~
          ( buttonMsgs Monome_256 False ++
            map (\xy -> ( (Monome_256, K.label)
                        , (xy, False)))
            (pcToXys_st st_0fs_1s pc1) )
          & stPending_Vivid .~
          [ ScAction_Free
            { _actionSynthDefEnum = Zot
            , _actionSynthName = v1 } ] )
