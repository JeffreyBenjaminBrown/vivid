{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Msg where

import Vivid
import Vivid.Jbb.Synths

-- | If needed, could use enum types instead of ParamName
type Msg = (String,Float)
