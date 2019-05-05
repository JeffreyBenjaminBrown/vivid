{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}

module Synths.Config where

import Vivid (I(..))


defaultAmp :: I "amp"
defaultAmp = 0.02

dirtSamplesFolder :: FilePath
dirtSamplesFolder = "/home/jeff/code/Tidal/Dirt-Samples"
