{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Montevideo.Monome.Test.Sustain where

import Test.HUnit

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import Montevideo.Monome.EdoMath
import Montevideo.Monome.Test.Data
import Montevideo.Monome.Util.Button
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Window.Keyboard as K
import Montevideo.Monome.Window.Sustain  as Su


tests :: Test
tests = TestList [
    TestLabel "test_sustainHandler" test_sustainHandler
  , TestLabel "test_deleteOneSustainedNote_and_insertOneSustainedNote"
    test_deleteOneSustainedNote_and_insertOneSustainedNote
  , TestLabel "test_toggleSustain" test_toggleSustain
  , TestLabel "test_voicesToSilence_uponSustainOff" test_voicesToSilence_uponSustainOff
  ]

test_voicesToSilence_uponSustainOff :: Test
test_voicesToSilence_uponSustainOff = TestCase $ do
  assertBool "Turn off sustain. Voice 0 is fingered, 1 is turned off." $
    voicesToSilence_uponSustainOff st_0fs_1s == S.singleton v1

test_toggleSustain :: Test
test_toggleSustain = TestCase $ do
  assertBool
    (unlines [
        "THE TEST: turn sustain on"
        , "THE ERROR: goes away if Monome.Config.edo = 31" ] ) $
    fromRight (error "bork") (toggleSustain st_0f) =^=
    ( st_0f & ( stApp . edoLit . at pc0 . _Just
                -- This is the only part that fails. Verify with:
                -- x = toggleSustain st_0f
                -- y = ( st_0f & ( stApp . edoLit . at pc0 . _Just
                --               %~ S.insert LedBecauseSustain )
                --       & stApp . edoSustaineded .~ Just (S.singleton v0 ) )
                -- x ^. stApp . edoLit . at pc0 . _Just
                -- y ^. stApp . edoLit . at pc0 . _Just
                -- x ^. stApp . edoSustaineded
                -- y ^. stApp . edoSustaineded
                %~ S.insert LedBecauseSustain )
            & stApp . edoSustaineded .~ Just (S.singleton v0 ) )

  assertBool "turn sustain off" $
    fromRight (error "bork") (toggleSustain st_0s)
    =^= ( st_0s
          & stApp . edoLit .~ mempty
          & stApp . edoSustaineded .~ Nothing )
  assertBool "turn sustain off, but finger persists" $
    fromRight (error "bork") (toggleSustain st_0fs)
    =^= ( st_0fs & ( stApp . edoLit . at pc0 . _Just
                     %~ S.delete LedBecauseSustain )
          & stApp . edoSustaineded .~ Nothing )

test_deleteOneSustainedNote_and_insertOneSustainedNote :: Test
test_deleteOneSustainedNote_and_insertOneSustainedNote = TestCase $ do
  let
    pc = 0
    lit_a :: Map (PitchClass EdoApp) (Set LedBecause) = -- lit b/c anchor
      M.singleton pc $ S.singleton LedBecauseAnchor
    lit_s :: Map (PitchClass EdoApp) (Set LedBecause) = -- lit b/c/ sustain
      M.singleton pc $ S.singleton LedBecauseSustain
    lit_as :: Map (PitchClass EdoApp) (Set LedBecause) = -- lit b/c both
      M.singleton pc $ S.fromList [LedBecauseAnchor, LedBecauseSustain]
  assertBool "add sustain to the reasons a lit (because anchored) key is lit"
    $ insertOneSustainedNote pc lit_a == lit_as
  assertBool "add sustain to the previously empty set of reasons a key is lit"
    $ insertOneSustainedNote pc mempty == lit_s

  assertBool "if sustain was the only reason, then upon releasing sustain, there are no more reasons" $
    deleteOneSustainedNote pc lit_s == mempty
  assertBool "if the anchor note is sustained, then upon releasing sustain, the anchor reemains as reason to light the key" $
    deleteOneSustainedNote pc lit_as == lit_a

test_sustainHandler :: Test
test_sustainHandler = TestCase $ do
  assertBool "releasing (not turning off) the sustain button has no effect"
    $ fromRight (error "bork")
    (Su.handler st0 (meh , False))
    =^= st0

  assertBool
    (unlines [
        "THE TEST: turning ON sustain changes the sustain state, the set of sustained voices, the set of reasons for keys to be lit, and the messages pending to the monome."
        , "THE ERROR: goes away if Monome.Config.edo = 31" ] ) $
    fromRight (error "bork")
    (Su.handler st_0f (meh, True))
    =^= (st_0f & stApp . edoSustaineded .~ Just (S.singleton v0)
          & ( -- This is the (only) part that fails. Verify failure with:
              -- x = Su.handler st_0f (meh, True) ^. stApp . edoLit
              -- y = M.singleton pc0 $ S.fromList [ LedBecauseSustain
              --                                  , LedBecauseSwitch xy0 ]
            stApp . edoLit .~ M.singleton pc0
              ( S.fromList [ LedBecauseSustain
                           , LedBecauseSwitch xy0 ] ) )
          & stPending_Monome .~
          [ (Su.label, (Su.theButton, True)) ] )

  assertBool ( "turning sustain OFF does all this stuff:\n" ++
               "flip the sustain state\n" ++
               "emptiy the set of sustained voices\n" ++
               "remove all `LedBecauseSustain`s from reasons for lit keys\n"
               ++ "adds messages for the monome to turn off the sustain button and the keys that were sustained and are not fingered\n" ++
               " adds messages for Vivid to turn off any pitches from voices that were sustained and are not fingered\n" ++
               "Pitch 0 is fingered, and 0 and 1 sounding; 1 turns off.") $
    fromRight (error "bork")
    (Su.handler st_0fs_1s (meh, True))
    =^= ( st_0fs_1s
          & stApp . edoSustaineded .~ mempty
          & stApp . edoLit .~ M.singleton pc0 ( S.singleton $
                                               LedBecauseSwitch xy0 )
          & stPending_Monome .~
          ( ( Su.label, (Su.theButton, False)) :
            map (\xy -> (K.label, (xy, False)))
            (pcToXys_st st_0fs_1s pc1) )
          & stPending_Vivid .~ [ SoundMsg { _soundMsgVoiceId = v1
                                          , _soundMsgVal = 0
                                          , _soundMsgParam = "amp" } ] )
