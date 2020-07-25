{-# LANGUAGE TupleSections
, ScopedTypeVariables #-}

module Montevideo.Monome.Test.Windows where

import Test.HUnit

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import qualified Data.Set as S

import Montevideo.Monome.EdoMath
import Montevideo.Monome.Test.Data
import Montevideo.Monome.Util.Button
import Montevideo.Monome.Types.Most
import Montevideo.Util
import Montevideo.Monome.Window.Common
import Montevideo.Monome.Window.Keyboard as K
import Montevideo.Monome.Window.Shift    as Sh


tests :: Test
tests = TestList [
    TestLabel "test_shiftHandler" test_shiftHandler
  , TestLabel "test_keyboardHandler" test_keyboardHandler
  ]

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
  assertBool
    (unlines [
        "THE TEST: releasing a key sends off-messages to monome, sends off-messages to Vivid, removes something from _edoFingers, and removes some things from _edoLit"
        , "THE ERROR: goes away if Monome.Config.edo = 31" ] ) $
    fromRight (error "bork")
    (K.handler st_01f (xy1, False))
    =^= ( st_0f
          & ( stPending_Monome .~
              -- This is the part that fails. Verify with this:
              -- x = Kb.handler st_01f (xy1, False) ^. stPending_Monome
              -- y = ( map (\xy -> (Kb.label, (xy, False)) )
              --   (pcToXys (st_01f ^. stApp . edoXyShift) pitch1 ) )
              map (\xy -> (K.label, (xy, False)) )
              (pcToXys_st st_01f pitch1 ) )
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v1
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's also the anchor pitch sends no monome messages" $
    fromRight (error "bork")
    (K.handler st_0af (xy0, False))
    =^= ( st_0af
          & ( stApp . edoLit . at pc0 . _Just
              .~ S.singleton LedBecauseAnchor )
          & stApp . edoFingers .~ mempty
          & stPending_Vivid .~ [SoundMsg { _soundMsgVoiceId = v0
                                         , _soundMsgPitch = Nothing
                                         , _soundMsgVal = 0
                                         , _soundMsgParam = "amp" } ] )

  assertBool "releasing a key that's a sustained voice sends no vivid or monome messages, but updates lit and fingers" $
    fromRight (error "bork")
    (K.handler st_0fs (xy0, False))
    =^= ( st_0fs
          & ( stApp . edoLit . at pc0 . _Just
              .~ S.singleton LedBecauseSustain )
          & stApp . edoFingers .~ mempty )

  assertBool "pressing a key that's a sustained voice updates edoFingers and edoLit" $
    fromRight (error "bork")
    (K.handler st_0s (xy0, True))
    =^= ( st_0s & ( stApp . edoLit . at pc0 . _Just
                    %~ S.insert (LedBecauseSwitch xy0) )
          & stApp . edoFingers .~ M.fromList [ (xy0,v0) ] )

  assertBool "pressing a key sends on-messages to monome, sends on-messages to Vivid, adds something to _edoFingers, and asdds something from _edoLit" $
    fromRight (error "bork")
    (K.handler st_0f (xy1, True))
    =^= ( st_01f
          & ( stPending_Monome .~
              ( map (\xy -> (K.label, (xy, True)) ) $
                pcToXys_st st_01f pitch1 ) )
          & stPending_Vivid .~ etKey_SoundMsg st0 (xy1,True) )
