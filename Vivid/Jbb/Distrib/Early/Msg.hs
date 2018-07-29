{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Early.Msg where

import Vivid


-- | == Messaging

data MsgEarly sdArgs where
  MsgEarly :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> MsgEarly sdArgs

set' :: VividAction m => Synth params -> MsgEarly params -> m ()
set' synth (MsgEarly m) = set synth m


-- | == Probably obsolete

-- vapMsg :: String -> Float -> Msg VapParams
-- vapMsg "freq" n = Msg (toI n :: I "freq")
-- vapMsg "amp" n = Msg (toI n :: I "amp")
-- vapMsg "saw" n = Msg (toI n :: I "saw")
-- vapMsg "delay-freq" n = Msg (toI n :: I "delay-freq")
-- vapMsg "delay-amp" n = Msg (toI n :: I "delay-amp")
-- vapMsg "fm-freq" n = Msg (toI n :: I "fm-freq")
-- vapMsg "fm-amp" n = Msg (toI n :: I "fm-amp")
-- vapMsg "fm2-freq" n = Msg (toI n :: I "fm2-freq")
-- vapMsg "fm2-amp" n = Msg (toI n :: I "fm2-amp")
-- vapMsg "nz-lpf" n = Msg (toI n :: I "nz-lpf")
-- 
-- sqfmMsg :: String -> Float -> Msg SqfmParams
-- sqfmMsg "freq" n = Msg (toI n :: I "freq")
-- sqfmMsg "amp" n = Msg (toI n :: I "amp")
-- sqfmMsg "width" n = Msg (toI n :: I "width")
-- sqfmMsg "width-vib-freq" n = Msg (toI n :: I "width-vib-freq")
-- sqfmMsg "width-vib-amp" n = Msg (toI n :: I "width-vib-amp")
-- 
-- boopMsg :: String -> Float -> Msg BoopParams
-- boopMsg "freq" n = Msg (toI n :: I "freq")
-- boopMsg "amp"  n = Msg (toI n :: I "amp" )
