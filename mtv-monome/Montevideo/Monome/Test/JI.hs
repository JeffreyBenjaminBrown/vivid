{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Montevideo.Monome.Test.JI where

import Test.HUnit

import           Data.Either
import qualified Data.Map as M

import qualified Montevideo.Monome.Config.Mtv as Config
import           Montevideo.Monome.Types
import           Montevideo.Util
import           Montevideo.Monome.Window.JI
import           Montevideo.Synth
import           Montevideo.Synth.Msg


tests :: Test
tests = TestList [
    TestLabel "test_jiFreq" test_jiFreq
  , TestLabel "test_jiKeySound" test_jiKeySound
  ]

ja :: JiApp
ja = JiApp { _jiFingers = error "meh"
           , _jiShifts = [1,3/2]
           , _jiGenerator = [1,5/4] }

test_jiKeySound :: Test
test_jiKeySound = TestCase $ do
  let f :: (X,Y) -> IO ()
      f xy = let
        Right freq = jiFreq ja xy
        in do
        assertBool "sound on" $ jiKey_ScAction ja (VoiceId 3) (xy,True)
          == [ ScAction_New
               { _actionSynthDefEnum = Zot
               , _actionSynthName = VoiceId 3
               , _actionScParams = M.fromList
                 [ ("freq", Config.freq * Config.jiTranspose * fr freq)
                 , ("amp", Config.amp) ] } ]
        assertBool "sound off" $ jiKey_ScAction ja (VoiceId 4) (xy,False)
          == [ ScAction_Free
               { _actionSynthDefEnum = Zot
               , _actionSynthName = VoiceId 4 } ]
  mapM_ f [(0,0), (1,1), (1,3)]

test_jiFreq :: Test
test_jiFreq = TestCase $ do
  assertBool "nothing is out of range" $
    isRight (jiFreq ja (10000,10000))

  assertBool "unit" $
    jiFreq ja (0,0) == Right 1
  assertBool "the other pitch in the generator" $
    jiFreq ja (1,0) == Right (5/4)
  assertBool "the other (non-unity) shift" $
    jiFreq ja (0,1) == Right (3/2)
  assertBool "shifted in both the generator and the shifts" $
    jiFreq ja (1,1) == Right (15/8)

  assertBool "octave" $
    jiFreq ja (0,2) == Right 2
  assertBool "octave + generator + shift" $
    jiFreq ja (1,3) == Right (15/4)
