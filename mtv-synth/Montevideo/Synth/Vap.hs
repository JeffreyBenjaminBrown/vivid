{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Montevideo.Synth.Vap where

import Vivid

import Montevideo.Synth.Config


type VapParams = '[ "freq",      "amp"
                  , "saw"     -- between 0 and 1, else gets loud fast
                  , "delay-freq", "delay-amp"
                  , "fm-freq",  "fm-amp"
                  , "fm2-freq", "fm2-amp"
                  , "nz-lpf"] -- nz-amp would be collinear with fm2-amp

vap :: SynthDef VapParams
vap = sd ( 0   :: I "freq"
         , toI maxAmp :: I "amp"
         , 0   :: I "saw"
         , 0   :: I "delay-freq"
         , 0   :: I "delay-amp"
         , 0   :: I "fm-freq"
         , 0   :: I "fm-amp"
         , 0   :: I "fm2-freq"
         , 0   :: I "fm2-amp"
         , 0   :: I "nz-lpf"
         ) $ do
  nz <- lpf (in_ whiteNoise, freq_ (V::V "nz-lpf"))
  fm <- (V::V "fm-amp") ~* (sinOsc $ freq_ (V::V "fm-freq"))
  fm2 <- (V::V "fm2-amp") ~* (sinOsc $ freq_ (V::V "fm2-freq"))
  aSin <- sinOsc (freq_  $ (V::V "freq") ~+ fm ~+ fm2 ~* nz)
  aSaw <- saw (freq_  $ (V::V "freq") ~+ fm ~+ fm2 ~* nz)
  carrier <- (V::V "amp") ~* (    (      (V :: V "saw" ) ~* aSaw)
                               ~+ ((1 ~- (V :: V "saw")) ~* aSin)
                             )
  fb <- carrier ~+ (V :: V "delay-amp") ~*
    ( lpf $ in_ $ delayL (in_ carrier
                         , maxDelaySecs_ 1
                         , delaySecs_ (V :: V "delay-freq") ) )
  out 0 [fb, fb]
