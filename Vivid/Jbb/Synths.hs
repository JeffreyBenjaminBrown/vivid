{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Synths where

import Vivid


-- | == Messaging

data Msg sdArgs where
  Msg :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg sdArgs

set' :: VividAction m => Synth params -> Msg params -> m ()
set' synth (Msg m) = set synth m


-- | == Synths

data SynthDefName = Boop | Vap | Sqfm

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


-- | = Vap

type VapParams = '["freq",      "amp"
                  , "fm-freq",  "fm-amp"
                  , "fm2-freq", "fm2-amp"
                  , "nz-amp",   "nz-lpf"]

vap :: SynthDef VapParams
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

vapMsg :: String -> Float -> Msg VapParams
vapMsg "freq" n = Msg (toI n :: I "freq")
vapMsg "amp" n = Msg (toI n :: I "amp")
vapMsg "fm-freq" n = Msg (toI n :: I "fm-freq")
vapMsg "fm-amp" n = Msg (toI n :: I "fm-amp")
vapMsg "fm2-freq" n = Msg (toI n :: I "fm2-freq")
vapMsg "fm2-amp" n = Msg (toI n :: I "fm2-amp")
vapMsg "nz-amp" n = Msg (toI n :: I "nz-amp")
vapMsg "nz-lpf" n = Msg (toI n :: I "nz-lpf")


-- | = Sqfm

type SqfmParams = '["freq","amp","width","width-vib"]

sqfm :: SynthDef SqfmParams
sqfm = sd ( 0   :: I "freq"
          , 0.1 :: I "amp"
          , 50  :: I "width"
          , 51  :: I "width-vib"
          ) $ do
  s0 <- sinOsc (freq_ (V::V "width-vib"))
  s1 <- sinOsc (freq_ (V::V "width")) ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]

sqfmMsg :: String -> Float -> Msg SqfmParams
sqfmMsg "freq" n = Msg (toI n :: I "freq")
sqfmMsg "amp" n = Msg (toI n :: I "amp")
sqfmMsg "width" n = Msg (toI n :: I "width")
sqfmMsg "width-vib" n = Msg (toI n :: I "width-vib")
