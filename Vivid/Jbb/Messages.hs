{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

module Vivid.Jbb.Messages where

import Vivid


data Msg where
  ParamMsg :: (VarList usedParams, Subset (InnerVars usedParams) allParams)
      => Synth allParams -> usedParams -> Msg
  FreeMsg :: Synth params -> Msg

send :: VividAction m => Msg -> m ()
send (ParamMsg synth params) = set synth params
send (FreeMsg synth) = free synth
