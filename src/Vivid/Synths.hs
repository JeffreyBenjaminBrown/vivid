{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Synths (
  module X
  , SynthDefEnum(..)
  , BoopParams
  , BoopParam(..)
  , boop
  , boopSaw
  , boopPulse
  , SqfmParams
  , SqfmParam(..)
  , sqfm
) where

import Vivid
import Vivid.Synths.Vap as X
import Vivid.Synths.Zot as X


-- | == Synths

data SynthDefEnum = Boop -- PITFALL ! keep alphabetically ordered
                    -- so that the derived Ord instance is predictable
                  | Sqfm
                  | Vap
                  | Zot
  deriving (Show,Eq,Ord)


-- | = Boop

type BoopParams = '["freq",    "amp",    "on"]
data BoopParam = BoopFreq | BoopAmp | BoopOn

boop :: SynthDef BoopParams
boop = sd ( 0    :: I "freq"
          , 0.01 :: I "amp"
          , 0    :: I "on"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   s2 <- s1 ~* lag ( in_ (V::V "on")
                   , lagSecs_ 0.01 )
   out 0 [s2, s2]

boopSaw :: SynthDef BoopParams
boopSaw = sd ( 0    :: I "freq"
             , 0.01 :: I "amp"
             , 1 :: I "on" -- TODO : use
             ) $ do
   s1 <- (V::V "amp") ~* saw (freq_ (V::V "freq"))
   out 0 [s1, s1]

boopPulse :: SynthDef BoopParams
boopPulse = sd ( 0    :: I "freq"
               , 0.01 :: I "amp"
               , 1 :: I "on" -- TODO : use
               ) $ do
   s1 <- (V::V "amp") ~* pulse (freq_ (V::V "freq"))
   out 0 [s1, s1]


-- | = Sqfm

type SqfmParams = '["freq","amp","width"
                   ,"width-vib-amp","width-vib-freq"]
data SqfmParam = SqfmFreq | SqfmAmp | SqfmWidth
               | SqfmWidthVibAmp | SqfmWidthVibFreq

sqfm :: SynthDef SqfmParams
sqfm = sd ( 0   :: I "freq"
          , 0.1 :: I "amp"
          , 50  :: I "width"
          , 51  :: I "width-vib-amp"
          , 51  :: I "width-vib-freq"
          ) $ do
  s0 <- (V::V "width-vib-amp") ~* sinOsc (freq_ (V::V "width-vib-freq"))
  s1 <- (V::V "width") ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]
