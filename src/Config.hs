{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Config where

import Vivid (I(..))


defaultAmp :: I "amp"
defaultAmp = 0.02

dirtSamplesFolder :: FilePath
dirtSamplesFolder = "/home/jeff/code/Tidal/Dirt-Samples"
