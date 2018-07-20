{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Msg where

import Vivid


-- | == Messaging

data Msg sdArgs where
  Msg :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg sdArgs

set' :: VividAction m => Synth params -> Msg params -> m ()
set' synth (Msg m) = set synth m
