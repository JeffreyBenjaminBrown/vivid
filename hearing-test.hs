-- | A way to discover an ear's maximum audible frequency,
-- or the minimum amplitude at which a frequency is audible.
-- I'm using it to see if my hearing is coming back
-- after I accidentally lodged wax against my eardrum.)

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

import Vivid
import Montevideo.Synth.Envelope


data Side = LeftSide | RightSide
  deriving (Show, Eq, Ord)

sin_right = sd ( 100 :: I "kHz"
               , 0 :: I "amp"
               -- The rest are for onOffEnvelope.
               , 1 :: I "on"
               , 0.05 :: I "att"
               , 0.05 :: I "rel"
               ) $ do
  amp <- biOp Max 0.8
         $ uOp Abs (V :: V "amp")
  kHz <- -- It might be cpu-wasteful that this is an audio-rate signal.
    (V :: V "kHz") ~* 1000
  signal <- (V :: V "amp")
         ~* sinOsc (freq_ kHz)
         ~* onOffEnvelope -- important to avoid clicks
  silence <- 0 ~* signal
  out 0 [silence, signal]

beep :: Float -> Float -> IO ()
beep kHz amp = do
  s <- synth sin_right ( toI kHz :: I "kHz"
                       , toI amp :: I "amp" )
  wait 0.9
  set s (toI 0 :: I "on")
  wait 0.1 -- b/c the release ("rel") takes 0.05 seconds.
  free s
