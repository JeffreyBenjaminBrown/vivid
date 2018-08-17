{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Dispatch.Msg (
  set'
  , boopMsg
  , sqfmMsg
  , vapMsg
  ) where

import Data.List (partition)
import Control.Lens (_1,_2,over)

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Dispatch.Types


set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m

--schedule :: SynthRegister -> [(Time,Action)] -> IO ()
--schedule reg evs = mapM f evs where
--  f 

--scheduleSend :: SynthRegister -> [(Time,Action)] -> IO ()
--scheduleSend reg (t, Send Boop name msg) =
--  boops reg

--sortActionsBySynth :: [(Time,Action)] -> (   [ (Time, Msg' BoopParams) ]
--                                           , [ (Time, Msg' SqfmParams) ]
--                                           , [ (Time, Msg' VapParams ) ] )
--sortActionsBySynth evs =
--  let (boops,evs') = partition ((==Boop) . fst . actionSynth . snd) evs
--      (sqfms,vaps) = partition ((==Sqfm) . fst . actionSynth . snd) evs'
--  in ( map (over _2 boopMsg) boops
--     , [], [] )
----     , map sqfmMsg sqfms
----     , map vapMsg  vaps )


-- | = per-synth boilerplate

boopMsg :: Msg -> Msg' BoopParams
boopMsg ("freq",n) = Msg' (toI n :: I "freq")
boopMsg ("amp",n) = Msg' (toI n :: I "amp")

sqfmMsg :: Msg -> Msg' SqfmParams
sqfmMsg ("freq",n) = Msg' (toI n :: I "freq")
sqfmMsg ("amp",n) = Msg' (toI n :: I "amp")
sqfmMsg ("width",n) = Msg' (toI n :: I "width")
sqfmMsg ("width-vib-amp",n) = Msg' (toI n :: I "width-vib-amp")
sqfmMsg ("width-vib-freq",n) = Msg' (toI n :: I "width-vib-freq")

vapMsg :: Msg -> Msg' VapParams
vapMsg ("freq",n) = Msg' (toI n :: I "freq")
vapMsg ("amp",n) = Msg' (toI n :: I "amp")
vapMsg ("saw",n) = Msg' (toI n :: I "saw")
vapMsg ("delay-freq",n) = Msg' (toI n :: I "delay-freq")
vapMsg ("delay-amp",n) = Msg' (toI n :: I "delay-amp")
vapMsg ("fm-freq",n) = Msg' (toI n :: I "fm-freq")
vapMsg ("fm-amp",n) = Msg' (toI n :: I "fm-amp")
vapMsg ("fm2-freq",n) = Msg' (toI n :: I "fm2-freq")
vapMsg ("fm2-amp",n) = Msg' (toI n :: I "fm2-amp")
vapMsg ("nz-lpf",n) = Msg' (toI n :: I "nz-lpf")
