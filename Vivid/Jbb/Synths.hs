{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Synths (
  module X
  , SynthDefName(..)
  , BoopParams
  , boop
  , boopMsg
  , SqfmParams
  , sqfm
  , sqfmMsg
) where

import Vivid
import Vivid.Jbb.Msg
import Vivid.Jbb.Synths.Vap as X
import Vivid.Jbb.Synths.Zot as X


-- | == Synths

data SynthDefName = Boop | Vap | Sqfm | Zot

-- | = Boop

type BoopParams = '["freq","amp"]

boop :: SynthDef BoopParams
boop = sd ( 0    :: I "freq"
          , 0.01 :: I "amp"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

boopMsg :: String -> Float -> Msg BoopParams
boopMsg "freq" n = Msg (toI n :: I "freq")
boopMsg "amp"  n = Msg (toI n :: I "amp" )


-- | = Sqfm

type SqfmParams = '["freq","amp","width","width-vib-amp","width-vib-freq"]

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

sqfmMsg :: String -> Float -> Msg SqfmParams
sqfmMsg "freq" n = Msg (toI n :: I "freq")
sqfmMsg "amp" n = Msg (toI n :: I "amp")
sqfmMsg "width" n = Msg (toI n :: I "width")
sqfmMsg "width-vib-freq" n = Msg (toI n :: I "width-vib-freq")
sqfmMsg "width-vib-amp" n = Msg (toI n :: I "width-vib-amp")
