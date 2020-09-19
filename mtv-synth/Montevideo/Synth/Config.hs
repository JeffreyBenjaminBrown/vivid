{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}

module Montevideo.Synth.Config where


-- | PITFALL: This is not a binding maximum,
-- just an arbitrary level that other things are defined in terms of.
-- SuperCollider distorts for values outside of [-1,1].
maxAmp :: Fractional a => a
maxAmp = 0.1

dirtSamplesFolder :: FilePath
dirtSamplesFolder = "/home/jeff/code/music/Tidal/Dirt-Samples"

filterFreqMax, filterFreqMin :: Num a => a

-- | This is the Nyquist frequency if sampling at 44.1 KHz.
-- Frequencies above this cannot be worked with.
filterFreqMax = 22050

-- | On the low side, human hearing only extends to about 40 Hz.
-- Some filters go crazy at values "close to zero".
-- I don't know what "close" means, but at least for the synths I've written
-- so far, I don't think I lose anything by flooring filter frequencies
-- at 10 Hz.
filterFreqMin = 10
