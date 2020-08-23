{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Montevideo.Synth (
  module X
  , SynthDefEnum(..)
  , BoopParams
  , boop
  , boopSaw
  , boopPulse
  , MoopParams
  , moop
  , SamplerParams
  , sampler
  , SqfmParams
  , sqfm
) where

import Vivid

import qualified Montevideo.Monome.Config.Mtv as Config
import           Montevideo.Synth.Samples
import           Montevideo.Synth.Vap as X
import           Montevideo.Synth.Zot as X
import           Montevideo.Synth.Config


-- | == Synths

data SynthDefEnum = -- PITFALL ! keep these alphabetically ordered
    Boop            -- so that the derived Ord instance is predictable
  | Moop
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


-- * Boop for the monome
type MoopParams = '["freq","amp"]

-- | PITFALL: A default freq of 0 might seem natural,
-- but that causes a popping sounds when it's changed.
moop :: SynthDef MoopParams
moop = sd ( toI Config.freq :: I "freq"
          , 0 :: I "amp"
          ) $ do
  -- p <- pulse (freq_ (V::V "freq"))
  -- s <- saw (freq_ (V::V "freq"))
  -- slow <- sinOsc (freq_ $ (V::V "freq") ~/ 100)
  -- sn2 <- sinOsc ( freq_ $ (V::V "freq") ~* (2 ~+ slow ~/ 60) )
         -- `slow` gives a slight vibrato effect
  sn  <- sinOsc (freq_ (V::V "freq"))
         -- sometimes pulse instead of sinOsc
  sn2 <- sinOsc (freq_ $ 2 ~* (V::V "freq"))
  sn3 <- sinOsc (freq_ $ 3 ~* (V::V "freq"))
  sn4 <- sinOsc (freq_ $ 4 ~* (V::V "freq"))
  -- sn5 <- sinOsc (freq_ $ 5 ~* (V::V "freq"))
  s1 <- lag (in_ (V::V "amp"), lagSecs_ 0.03)
        -- The lag smooths out discontinuities in the change in "amp".
        ~* 0.05 -- to prevent distortion
        ~* foldr1 (~+) ( map (\(f,a) -> f ~* a)
                         [ (sn,1)
                         , (sn2,3/4)
                         , (sn3,1/5)
                         , (sn4,1/2)
                         --, (sn5,1/16)
                         ] )
  out 0 [s1, s1]


-- | = Sample

type SamplerParams = '["amp","buffer","speed","trigger"]

-- | Quoting the SC manual (https://doc.sccode.org/Classes/PlayBuf.html):
-- "A trigger causes a jump to the startPos. A trigger occurs when a signal changes from negative value to positive value."
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
