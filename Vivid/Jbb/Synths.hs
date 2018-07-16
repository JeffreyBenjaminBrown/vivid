{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Vivid.Jbb.Synths where

import Vivid


boop :: SynthDef '["freq","amp"]
boop = sd ( 0    :: I "freq"
          , 0.01 :: I "amp"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]


vap :: SynthDef '["freq",      "amp"
                 , "fm-freq",  "fm-amp"
                 , "fm2-freq", "fm2-amp"
                 , "nz-amp",   "nz-lpf"]
vap = sd ( 0   :: I "freq"
         , 0.1 :: I "amp"
         , 0   :: I "fm-freq"
         , 0   :: I "fm-amp"
         , 0   :: I "fm2-freq"
         , 0   :: I "fm2-amp"
         , 0   :: I "nz-amp"
         , 0   :: I "nz-lpf"
         ) $ do
  nz <- (V::V "nz-amp") ~* lpf (in_ whiteNoise, freq_ (V::V "nz-lpf"))
  fm <- (V::V "fm-amp") ~* (sinOsc $ freq_ (V::V "fm-freq"))
  fm2 <- (V::V "fm2-amp") ~* (sinOsc $ freq_ (V::V "fm2-freq"))
  carrier <- (V::V "amp")
    ~* sinOsc (freq_  $ (V::V "freq") ~+ fm ~+ fm2 ~+ nz)
  out 0 [carrier, carrier]

sqfm :: SynthDef '["freq","amp","width","width-vib"]
sqfm = sd ( 0 :: I "freq"
          , 0.1 :: I "amp"
          , 50 :: I "width"
          , 51 :: I "width-vib"
          ) $ do
  s0 <- sinOsc (freq_ (V::V "width_vib"))
  s1 <- sinOsc (freq_ (V::V "width")) ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]
