module Vivid.Jbb.Random.Synth where

import Data.Map as M

import Vivid
import Vivid.Jbb.Random.Signal


type AbSynth = M.Map AbSigName AbSig

randAbSynth :: RandConstraints -> IO AbSynth
randAbSynth cs = go cs M.empty where
  go :: RandConstraints -> AbSynth -> IO AbSynth
  go cs m = if namedSignals cs >= maxSignals cs
            then return m
            else do s <- randAbSig cs
                    let namedSignals' = namedSignals cs + 1
                        cs' = cs {namedSignals = namedSignals'}
                        m' = M.insert (sigName cs' namedSignals') s m
                    go cs' m'
