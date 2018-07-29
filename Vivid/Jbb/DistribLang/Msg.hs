{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.DistribLang.Msg where

import Vivid
import Vivid.Jbb.Synths

-- | If needed, could use enum types instead of ParamName
data Msg = Boop BoopParam Float
         | Vap  VapParam Float
         | Sqfm SqfmParam Float
         -- | Zot ZotParam Float -- todo

