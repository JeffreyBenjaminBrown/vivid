{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Montevideo.Synths (
  module X
  , SynthDefEnum(..)
  , BoopParams
  , boop
  , boopSaw
  , boopPulse
  , SamplerParams
  , sampler
  , SqfmParams
  , sqfm
) where

import Vivid

import Montevideo.Synths.Samples
import Montevideo.Synths.Vap as X
import Montevideo.Synths.Zot as X
import Montevideo.Synths.Config


-- | == Synths

data SynthDefEnum = -- PITFALL ! keep these alphabetically ordered
    Boop            -- so that the derived Ord instance is predictable
  | Sampler Sample -- ^ Sample = what sample it's playing
  | Sqfm
  | Vap
  | Zot
  deriving (Show,Eq,Ord)


-- | = Boop

type BoopParams = '["freq",    "amp",    "on"]

boop :: SynthDef BoopParams
boop = sd ( 0              :: I "freq"
          , toI defaultAmp :: I "amp"
          , 0              :: I "on"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   s2 <- s1 ~* lag ( in_ (V::V "on")
                   , lagSecs_ 0.01 )
   out 0 [s2, s2]

boopSaw :: SynthDef BoopParams
boopSaw = sd ( 0              :: I "freq"
             , toI defaultAmp :: I "amp"
             , 1              :: I "on" -- TODO : use
             ) $ do
   s1 <- (V::V "amp") ~* saw (freq_ (V::V "freq"))
   out 0 [s1, s1]

boopPulse :: SynthDef BoopParams
boopPulse = sd ( 0              :: I "freq"
               , toI defaultAmp :: I "amp"
               , 1              :: I "on" -- TODO : use
               ) $ do
   s1 <- (V::V "amp") ~* pulse (freq_ (V::V "freq"))
   out 0 [s1, s1]


-- | = Sample

type SamplerParams = '["amp","buffer","speed","trigger"]

sampler :: SynthDef SamplerParams
sampler = sd ( toI defaultAmp :: I "amp"
             , 0              :: I "buffer"
             , 1              :: I "speed"
             , 1              :: I "trigger" ) $ do
  let buffer = V::V "buffer"
  s <- (6 ~* (V::V "amp")) ~*
    playBuf ( trigger_ (V::V"trigger")
            , buf_ buffer
            , rate_ $ bufRateScale buffer ~* (V::V"speed")
            , doneAction_ (0::Int) -- don't disappear when sample finishes
            )
  out (0::Int) [s,s]


-- | = Sqfm

type SqfmParams = '["freq","amp","width"
                   ,"width-vib-amp","width-vib-freq"]

sqfm :: SynthDef SqfmParams
sqfm = sd ( 0              :: I "freq"
          , toI defaultAmp :: I "amp"
          , 50             :: I "width"
          , 51             :: I "width-vib-amp"
          , 51             :: I "width-vib-freq"
          ) $ do
  s0 <- (V::V "width-vib-amp") ~* sinOsc (freq_ (V::V "width-vib-freq"))
  s1 <- (V::V "width") ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]
