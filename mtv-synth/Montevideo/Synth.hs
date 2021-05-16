-- | PITFALL: The meanings recorded here of these messages are surely stale.
--
-- "on" permits, in non-Sampler synths,
--   separate amplitude and existence control.
--   It isn't used in Sampler.
--   In all the other synths, it is important to Dispatch,
--   because Dispatch voices are persistent even after "note-off"
--   ("on"=0) events. (TODO ? It seems like "amp" would work just as well,
--   wouldn't it?)
--
-- "trigger:" In the Sampler synth only,
--   whenver the user schedules a "trigger=1" message in Montevideo.Dispatch,
--   it automatically schedules a "trigger=0" for soon thereafter.
--   This permits the buffer to be retriggered the next time.
--
-- "lag" determines, in some synths, how long* a change in amplitude takes.
--   It was initially implemented to smooth out clicks.
--   It also can work as an envelope control for start, end or both.
--   If "amp" is lagged, but it begins as a positive value,
--   then it seems like it should click. Maybe the reason that doesn't happen
--   is that waveforms are normalized to always start at 0.
--   But it definitely works to smooth out an ending click,
--   assuming the voice is not freed until after the decay.
--
--   * It's implemented via a filter; really what it means is how long
--     it takes to be very close to the target value, something like -60 db.)

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Montevideo.Synth (
    module Montevideo.Synth.Vap
  , module Montevideo.Synth.Zot
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
import           Montevideo.Synth.Config
import           Montevideo.Synth.Envelope
import           Montevideo.Synth.Samples
import           Montevideo.Synth.Vap
import           Montevideo.Synth.Zot


-- | == Synths

data SynthDefEnum = -- PITFALL ! keep these alphabetically ordered
    Boop            -- so that the derived Ord instance is predictable
  | Moop
  | Sampler Sample -- ^ Sample = the sound file it plays
  | Sqfm
  | Vap
  | Zot
  deriving (Show,Eq,Ord)


-- | = Boop

type BoopParams = '["freq", "amp", "on","att","rel"]

boop :: SynthDef BoopParams
boop = sd ( 0          :: I "freq"
          , toI maxAmp :: I "amp"
          , 0          :: I "on"
          , 0          :: I "att"
          , 0          :: I "rel"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   s2 <- s1
         ~* lag ( in_ (V::V "on")
                , lagSecs_ 0.01 )
         ~* onOffEnvelope
   out 0 [s2, s2]

boopSaw :: SynthDef BoopParams
boopSaw = sd ( 0          :: I "freq"
             , toI maxAmp :: I "amp"
             , 1          :: I "on" -- TODO : use
             , 0          :: I "att"
             , 0          :: I "rel"
             ) $ do
   s1 <- (V::V "amp") ~* saw (freq_ (V::V "freq"))
   out 0 [s1, s1]

boopPulse :: SynthDef BoopParams
boopPulse = sd ( 0          :: I "freq"
               , toI maxAmp :: I "amp"
               , 1          :: I "on" -- TODO : use
               , 0          :: I "att"
               , 0          :: I "rel"
               ) $ do
   s1 <- (V::V "amp") ~* pulse (freq_ (V::V "freq"))
   out 0 [s1, s1]


-- * Boop for the monome
type MoopParams = '["freq", "amp", "lag"]

-- | PITFALL: A default freq of 0 might seem natural,
-- but that causes a popping sounds when it's changed.
moop :: SynthDef MoopParams
moop = sd ( toI Config.freq :: I "freq"
          , 0 :: I "amp"
          , 0.1 :: I "lag" -- measured in seconds
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
  s1 <- lag (in_ (V::V "amp"), lagSecs_ (V::V "lag"))
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
sampler = sd ( toI maxAmp :: I "amp"
             , 0          :: I "buffer"
             , 1          :: I "speed"
             , 1          :: I "trigger" ) $ do
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
sqfm = sd ( 0          :: I "freq"
          , toI maxAmp :: I "amp"
          , 50         :: I "width"
          , 51         :: I "width-vib-amp"
          , 51         :: I "width-vib-freq"
          ) $ do
  s0 <- (V::V "width-vib-amp") ~* sinOsc (freq_ (V::V "width-vib-freq"))
  s1 <- (V::V "width") ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]
