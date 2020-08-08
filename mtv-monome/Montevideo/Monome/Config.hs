-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where

import Montevideo.Monome.Types.EdoConfig


-- | * Configure the tuning

-- | Change these values to tune the synth.
-- See comments in Montevideo.Monome.Types.EdoConfig to see what they do.
theConfig :: EdoConfig
theConfig = EdoConfig
  { _edo = 46
  , _spacing = 9
  , _skip = 1
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
