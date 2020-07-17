-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where

import Montevideo.Monome.Types.EdoConfig


-- | * Configure the tuning

-- | Change these values to tune the synth.
theConfig = EdoConfig
  { _edo = 46
  , _spacing = 6
  , _skip = 1
  , _octaveStretchInCents = 0
  }

-- | DEPRECATED: These should go away.
edo                  =  _edo                  theConfig
spacing              =  _spacing              theConfig
skip                 =  _skip                 theConfig
octaveStretchInCents =  _octaveStretchInCents theConfig


-- | * Configure the synth

-- | The amplitude of each voice.
amp :: Float
amp = 0.35

-- | The frequency of "note 0". (Notes can be negative.)
freq :: Num a => a
freq = 80
