-- * Configure `Montevideo.Monome` -- how the synth sounds,
-- and how the monome is interpreted.

module Montevideo.Monome.Config.Mtv where

import Montevideo.Monome.Types.EdoConfig


-- | * Configure the tuning.
--
-- An `EdoConfig` is one of the arguments to `Monome.Main.edoMonome`.
-- Below are my favorites.




--    , tReport_modulus = 11
--    , tReport_spacing = 15
--        [ 56 steps = 2 % 1: string 3 fret 1
--        , 45 steps = 7 % 4: string 3 fret 0
--        , 33 steps = 3 % 2: string 0 fret 3
--        , 26 steps = 11 % 8: string 1 fret 1
--        , 18 steps = 5 % 4: string -1 fret 3
--
--    , tReport_modulus = 4
--    , tReport_spacing = 15
--        [ 56 steps = 2 % 1: string 4 fret -1
--        , 45 steps = 7 % 4: string 3 fret 0
--        , 33 steps = 3 % 2: string 3 fret -3
--        , 26 steps = 11 % 8: string 2 fret -1
--        , 18 steps = 5 % 4: string 2 fret -3
--
--    , tReport_modulus = 8
--    , tReport_spacing = 13
--        [ 81 steps = 2 % 1: string 5 fret 2
--        , 65 steps = 7 % 4: string 5 fret 0
--        , 47 steps = 3 % 2: string 3 fret 1
--        , 37 steps = 11 % 8: string 1 fret 3
--        , 26 steps = 5 % 4: string 2 fret 0
--
--    -- TODO: My favorite 53 so far.
--    , tReport_modulus = 5
--    , tReport_spacing = 12
--        [ 53 steps = 2 % 1: string 4 fret 1
--        , 43 steps = 7 % 4: string 4 fret -1
--        , 37 steps = 13 % 8: string 1 fret 5
--        , 31 steps = 3 % 2: string 3 fret -1
--        , 24 steps = 11 % 8: string 2 fret 0
--        , 17 steps = 5 % 4: string 1 fret 1
--        ]
--
--    , tReport_modulus = 7
--    , tReport_spacing = 8
--        [ 53 steps = 2 % 1: string 4 fret 3
--        , 43 steps = 7 % 4: string 1 fret 5
--        , 37 steps = 13 % 8: string 2 fret 3
--        , 31 steps = 3 % 2: string 3 fret 1
--        , 24 steps = 11 % 8: string 3 fret 0
--        , 17 steps = 5 % 4: string 3 fret -1
--        ]
--
--    -- TODO : Lots of other 68 tunings look plausible, too.
--    , tReport_modulus = 2
--    , tReport_spacing = 11
--        [ 68 steps = 2 % 1: string 6 fret 1
--        , 55 steps = 7 % 4: string 5 fret 0
--        , 48 steps = 13 % 8: string 4 fret 2
--        , 40 steps = 3 % 2: string 4 fret -2
--        , 31 steps = 11 % 8: string 3 fret -1
--        , 22 steps = 5 % 4: string 2 fret 0
--
--    , tReport_modulus = 3
--    , tReport_spacing = 11
--        [ 72 steps = 2 % 1: string 6 fret 2
--        , 58 steps = 7 % 4: string 5 fret 1
--        , 50 steps = 13 % 8: string 4 fret 2
--        , 42 steps = 3 % 2: string 3 fret 3
--        , 33 steps = 11 % 8: string 3 fret 0
--        , 23 steps = 5 % 4: string 1 fret 4


-- | 77 steps = 2 % 1: string 5 fret 4
--   62 steps = 7 % 4: string 5 fret -1
--   54 steps = 13 % 8: string 3 fret 5
--   45 steps = 3 % 2: string 3 fret 2
--   35 steps = 11 % 8: string 2 fret 3
--   25 steps = 5 % 4: string 1 fret 4

my77 :: EdoConfig
my77 = EdoConfig
  { _edo = 77
  , _skip = 3
  , _spacing = 13
  , _octaveStretchInCents = 0.222
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,13)
      , _gridHorizontalVector = (5,4)
      }
  }

-- | 84 steps = 2 % 1  : string 6 fret 2
--   68 steps = 7 % 4  : string 5 fret 1
--   59 steps = 13 % 8 : string 5 fret -2
--   49 steps = 3 % 2  : string 4 fret -1
--   39 steps = 11 % 8 : string 3 fret 0
--   27 steps = 5 % 4  : string 3 fret -4

my84 :: EdoConfig
my84 = EdoConfig
  { _edo = 84
  , _skip = 3
  , _spacing = 13
  , _octaveStretchInCents = -0.292
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,13)
      , _gridHorizontalVector = (6,2)
      }
  }

-- | 90 steps = 2 % 1  : string 5 fret 1
--   73 steps = 7 % 4  : string 4 fret 1
--   63 steps = 13 % 8 : string 4 fret -1
--   53 steps = 3 % 2  : string 4 fret -3
--   41 steps = 11 % 8 : string 3 fret -2
--   29 steps = 5 % 4  : string 2 fret -1
-- The difficulty with this tuning is that 9/8 is an enormous interval.
-- Otherwise it's pretty nice.

my90 :: EdoConfig
my90 = EdoConfig
  { _edo = 90
  , _skip = 5
  , _spacing = 17
  , _octaveStretchInCents = -0.542
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-5,17)
      , _gridHorizontalVector = (5,1)
      }
  }

-- | 105 steps = 2 % 1  : string 5 fret 2
--   85 steps  = 7 % 4  : string 5 fret 0
--   74 steps  = 13 % 8 : string 2 fret 4
--   61 steps  = 3 % 2  : string 3 fret 1
--   48 steps  = 11 % 8 : string 4 fret -2
--   34 steps  = 5 % 4  : string 2 fret 0
my105 :: EdoConfig
my105 = EdoConfig
  { _edo = 105
  , _skip = 10
  , _spacing = 17
  , _octaveStretchInCents = 0.086
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-10,17)
      , _gridHorizontalVector = (5,2)
      }
  }

-- | 118 steps = 2 % 1  : string 6t fret 1
--   95 steps  = 7 % 4  : string 5 fret 0
--   83 steps  = 13 % 8 : string 5 fret -3
--   69 steps  = 3 % 2  : string 3 fret 3
--   54 steps  = 11 % 8 : string 2 fret 4
--   38 steps  = 5 % 4  : string 2 fret 0
my118 :: EdoConfig
my118 = EdoConfig
  { _edo = 118
  , _skip = 4
  , _spacing = 19
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-4,19)
      , _gridHorizontalVector = (6,1)
      }
  }

-- | 118 steps = 2 % 1  : string 6 fret -5
--   95 steps  = 7 % 4  : string 5 fret -5
--   83 steps  = 13 % 8 : string 5 fret -8
--   69 steps  = 3 % 2  : string 3 fret 0
--   54 steps  = 11 % 8 : string 2 fret 2
--   38 steps  = 5 % 4  : string 2 fret -2
my118_diagonal :: EdoConfig
my118_diagonal = EdoConfig
  { _edo = 118
  , _skip = 4
  , _spacing = 23
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-4,23)
      , _gridHorizontalVector = (6,-5)
      }
  }

my87 :: EdoConfig
my87 = EdoConfig
  { _edo = 87
  , _skip = 2
  , _spacing = 17
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,17)
      , _gridHorizontalVector = (5,1)
      }
  }

-- | Consistent through the 19-limit,
-- and very accurate even through the 31-limit (the highest I checked).
-- The maximum possible error for a note is 7.5 cents.
-- Note positions:
-- 07 steps = 17 % 16: string 1 fret -3
-- 20 steps = 19 % 16: string 2 fret -3
-- 26 steps = 5 % 4: string 2 fret 0
-- 37 steps = 11 % 8: string 3 fret -1
-- 42 steps = 23 % 16: string 2 fret 8
-- 47 steps = 3 % 2: string 3 fret 4
-- 56 steps = 13 % 8: string 4 fret 2
-- 65 steps = 7 % 4: string 5 fret 0
-- 69 steps = 29 % 16: string 5 fret 2
-- 76 steps = 31 % 16: string 6 fret -1
-- 80 steps = 2 % 1: string 6 fret 1
--Prime accuracy:
-- (3 % 2,(47,7050,30.4499913461259))
-- (5 % 4,(26,3900,36.862861351651645))
-- (7 % 4,(65,9750,61.74093530875143))
-- (11 % 8,(37,5550,36.82057635243291))
-- (13 % 8,(56,8400,-5.276617693105436))
-- (17 % 16,(7,1050,0.4459049959270942))
-- (19 % 16,(20,3000,24.869838676973814))
-- (23 % 16,(42,6300,17.256527315844323))
-- (29 % 16,(69,10350,54.22805846913434))
-- (31 % 16,(76,11400,-50.35572464250254))
my80 :: EdoConfig
my80 = EdoConfig
  { _edo = 80
  , _skip = 2
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,13)
      , _gridHorizontalVector = (6,1)
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


-- | 41 steps = 2 % 1  : string 3 fret 1
--   33 steps = 7 % 4  : string 3 fret -3
--   29 steps = 13 % 8 : string 3 fret -5
--   24 steps = 3 % 2  : string 2 fret -1
--   19 steps = 11 % 8 : string 1 fret 3
--   13 steps = 5 % 4  : string 1 fret 0
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
  , _spacing = 7
  , _octaveStretchInCents =
    -1.1    -- 11-limit optimal
    -- -1.8 -- 7-limit optimal
  , _gridVectors = Nothing
  }

my19 :: EdoConfig
my19 = EdoConfig
  { _edo = 19
  , _skip = 1
  , _spacing = 6
  , _octaveStretchInCents =
      2.58    -- 5-limit optimal
      -- 3.85 -- 7-limit optimal
  , _gridVectors = Nothing
  }

my16 :: EdoConfig
my16 = EdoConfig
  { _edo = 16
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents =
      7.3    -- 5-limit optimal
  , _gridVectors = Nothing
  }

my12 :: EdoConfig
my12 = EdoConfig
  { _edo = 12
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }


-- | * Configure the synth

-- | The amplitude of each voice.
amp :: Float
amp = 0.1

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
freeDelay = 3
