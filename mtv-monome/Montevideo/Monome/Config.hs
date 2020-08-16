-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where

import Montevideo.Monome.Types.EdoConfig


-- | * Configure the tuning

-- | Change these values to tune the synth.
-- See comments in Montevideo.Monome.Types.EdoConfig to see what they do.
-- Some tunings I like:
-- 22 1 5  -1.1
-- 31 1 6   0.5
-- 41 1 8   0
-- 41 2 13  0 (the Kite Guitar tuning)
-- 46 1 9   0
-- 87 2 17  0

theConfig :: EdoConfig
theConfig = EdoConfig
  { _edo = 46
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0
  }


-- | * Configure the synth

-- | The amplitude of each voice.
amp :: Float
amp = 0.35

-- | The frequency of "note 0". (Notes can be negative.)
freq :: Num a => a
freq = 80

-- | The JI app's notes are too high without scaling by a factor like this.
jiTranspose :: Fractional a => a
jiTranspose = (1/32)
