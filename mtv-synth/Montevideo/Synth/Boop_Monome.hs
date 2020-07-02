{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Montevideo.Synth.Boop_Monome (
  BoopParams
  , boop
  ) where

import Vivid

import qualified Montevideo.Monome.Config as Config


type BoopParams = '["freq","amp"]

-- | PITFALL: A default freq of 0 might seem natural,
-- but that causes a popping sounds when it's changed.
boop :: SynthDef BoopParams
boop = sd ( toI Config.freq :: I "freq"
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
  sn5 <- sinOsc (freq_ $ 5 ~* (V::V "freq"))
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
