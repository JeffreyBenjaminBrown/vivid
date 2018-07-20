{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Synths.Vap where

import Vivid
import Vivid.Jbb.Msg


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
