-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where

import Montevideo.Monome.Types.EdoConfig


-- | * Configure the tuning



-- 22 1 5  -1.1
-- 31 1 6   0.5
-- 41 1 8   0
-- 41 2 13  0 (the Kite Guitar tuning)
-- 46 1 9   0
-- 87 2 17  0


my87 :: EdoConfig
my87 = EdoConfig
  { _edo = 87
  , _skip = 2
  , _spacing = 17
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,17)
      , _gridHorizontalVector = (5,1)
      }
  }

my46 :: EdoConfig
my46 = EdoConfig
  { _edo = 46
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

kiteGuitar :: EdoConfig
kiteGuitar = EdoConfig
  { _edo = 41
  , _skip = 2
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = ( -2, 13 )
      , _gridHorizontalVector = (3,1)
      }
  }

my41 :: EdoConfig
my41 = EdoConfig
  { _edo = 41
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my31 :: EdoConfig
my31 = EdoConfig
  { _edo = 31
  , _skip = 1
  , _spacing = 6
  , _octaveStretchInCents = 0.5
  , _gridVectors = Nothing
  }

my22 :: EdoConfig
my22 = EdoConfig
  { _edo = 22
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents = -1.1
  , _gridVectors = Nothing
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

-- | How long to wait between zeroing a voice's volume and freeing it in SC.
-- TODO : implement reverb tails, which will require some new signal
-- aside from amp=0, because amp=0 takes effect immediately.
freeDelay :: Float
freeDelay = 0.1
