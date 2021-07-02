-- * Configure `Montevideo.Monome` -- how the synth sounds,
-- and how the monome is interpreted.

module Montevideo.Monome.Config.Mtv where

import Montevideo.Monome.Types.Edo


-- | * Configure the tuning.
--
-- An `MonomeEdo` is one of the arguments to `Monome.Main.edoMonome`.
-- Below are my favorites.


my50_3_10 :: MonomeEdo
my50_3_10 = MonomeEdo
  { _edo = 50
  , _skip = 3
  , _spacing = 10
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,10)
      , _gridHorizontalVector = (5,0)
      }
  }

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

-- |
--   13 steps = 19 % 16   : string 1 fret 1
--   17 steps = 5 % 4     : string 1 fret 3
--   24 steps = 11 % 8    : string 2 fret 1 -- -7.9c error
--   31 steps = 3 % 2     : string 3 fret -1
--   37 steps = 13 % 8    : string 3 fret 2
--   51 steps = 31 % 16   : string 5 fret -2 -- +10c error
--   53 steps = 2 % 1     : string 5 fret -1 -- +5.7c error
-- harder to reach:
--   5 steps = 17 % 16    : string 1 fret -3 -- +8.2c error
--   28 steps = 23 % 16   : string 2 fret 3
--   43 steps = 7 % 4     : string 3 fret 5 -- +4.7 cents error
--   45 steps = 29 % 16   : string 3 fret 6 -- -11 cents error

my53_2_11 :: MonomeEdo
my53_2_11 = MonomeEdo
  { _edo = 53
  , _skip = 2
  , _spacing = 11
  , _octaveStretchInCents = 0.74 -- optimal for the  2 3 5 11 13 19 subgroup,
  -- which happen to be all the easiest-reached notes.
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,8)
      , _gridHorizontalVector = (7,2)
      }
  }

-- | Beautiful 5-limit layout.
-- Take 2nd-best primes 7 and 11, and it's identical to 34 2 9.

my53_3_14 :: MonomeEdo
my53_3_14 = MonomeEdo
  { _edo = 53
  , _skip = 3
  , _spacing = 14
  , _octaveStretchInCents = 0.74 -- optimal for the  2 3 5 11 13 19 subgroup,
  -- which happen to be all the easiest-reached notes.
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,14)
      , _gridHorizontalVector = (4,-1)
      }
  }

-- | 58 steps = 2 % 1   : string 5 fret 1
--   34 steps = 3 % 2   : string 2 fret 4
--   19 steps = 5 % 4   : string 2 fret - 1
--   47 steps = 7 % 4   : string 4 fret 1
--   27 steps = 11 % 8  : string 3 fret - 2
--   41 steps = 13 % 8  : string 4 fret - 1
--   5 steps = 17 % 16  : string 1 fret - 2
--   14 steps = 19 % 16 : string 1 fret 1
--   30 steps = 23 % 16 : string 3 fret - 1
--   50 steps = 29 % 16 : string 4 fret 2
--   55 steps = 31 % 16 : string 5 fret 0

my58_3_11 :: MonomeEdo
my58_3_11 = MonomeEdo
  { _edo = 58
  , _skip = 3
  , _spacing = 11
  , _octaveStretchInCents = -1.694
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,11)
      , _gridHorizontalVector = (5,1)
      }
  }

-- | 58 edo: Looks goofy but the errors cancel.
-- With a Kite-ish tuning it's pretty familiar,
-- gaining some things and losing only the easy-to-reach 11%8.
-- *** just low (through 13) or easy primes
-- 19 steps = 5 % 4   : str 1 fret 3
-- 27 steps = 11 % 8  : str 3 fret -6 | str 1 fret 7
-- 30 steps = 23 % 16 : str 2 fret 2
-- 34 steps = 3 % 2   : str 2 fret 4
-- 41 steps = 13 % 8  : str 3 fret 1
-- 47 steps = 7 % 4   : str 3 fret 4
-- 50 steps = 29 % 16 : str 4 fret -1
-- 58 steps = 2 % 1   : str 4 fret 3
-- *** all the primes
--  5 steps = 17 % 16 : str 1 fret -4 | str -1 fret 9
-- 14 steps = 19 % 16 : str 2 fret -6 | str 0 fret 7
-- 19 steps = 5 % 4   : str 1 fret 3
-- 27 steps = 11 % 8  : str 3 fret -6 | str 1 fret 7
-- 30 steps = 23 % 16 : str 2 fret 2
-- 34 steps = 3 % 2   : str 2 fret 4
-- 41 steps = 13 % 8  : str 3 fret 1
-- 47 steps = 7 % 4   : str 3 fret 4
-- 50 steps = 29 % 16 : str 4 fret -1
-- 55 steps = 31 % 16 : str 5 fret -5 | str 3 fret 8
-- 58 steps = 2 % 1   : str 4 fret 3
my58_2_13 :: MonomeEdo
my58_2_13 = MonomeEdo
  { _edo = 58
  , _skip = 2
  , _spacing = 13
  , _octaveStretchInCents = -1.694
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,13)
      , _gridHorizontalVector = (4,3)
      }
  }


-- | 58 edo with 8 steps between "strings":
-- 58 steps = 02 % 1  : str 7 fret 2
-- 34 steps = 03 % 2  : str 4 fret 2
-- 19 steps = 05 % 4  : str 2 fret 3
-- 47 steps = 07 % 4  : str 6 fret -1
-- 27 steps = 11 % 8  : str 3 fret 3
-- 41 steps = 13 % 8  : str 5 fret 1
--  5 steps = 17 % 16 : str 1 fret -3 | str 0 fret 5
-- 14 steps = 19 % 16 : str 2 fret -2 | str 1 fret 6
-- 30 steps = 23 % 16 : str 4 fret -2 | str 3 fret 6
-- 50 steps = 29 % 16 : str 6 fret 2
-- 55 steps = 31 % 16 : str 7 fret -1

my58 :: MonomeEdo
my58 = MonomeEdo
  { _edo = 58
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = -1.694
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,8)
      , _gridHorizontalVector = (7,2)
      }
  }

-- | 62 steps; 2  % 1 ;  string 4 ; fret 2
--   36 steps; 3  % 2 ;  string 3 ; fret -2
--   20 steps; 5  % 4 ;  string 1 ; fret 2
--   50 steps; 7  % 4 ;  string 4 ; fret -2
--   11 steps; 9  % 8 ;  string 1 ; fret -1
--   28 steps; 11 % 8 ;  string 2 ; fret 0
--   43 steps; 13 % 8 ;  string 2 ; fret 5
--   56 steps; 15 % 8 ;  string 4 ; fret 0
--    5 steps; 17 % 16 ; string 1 ; fret -3
--   15 steps; 19 % 16 ; string 0 ; fret 5
--   24 steps; 21 % 16 ; string 0 ; fret 8
--   32 steps; 23 % 16 ; string 1 ; fret 6
--   40 steps; 25 % 16 ; string 2 ; fret 4
--   47 steps; 27 % 16 ; string 4 ; fret -3
--   53 steps; 29 % 16 ; string 4 ; fret -1
--   59 steps; 31 % 16 ; string 4 ; fret 1

my62 :: MonomeEdo
my62 = MonomeEdo
  { _edo = 62
  , _skip = 3
  , _spacing = 14
  , _octaveStretchInCents = 1.4 -- optimal for the 31-limit
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,14)
      , _gridHorizontalVector = (4,2)
      }
  }

-- | 63-edo, mod 1 space 9
-- PITFALL: inconsistent: 16/9 ~ 29/16 ~ 53\63, whereas
--          (3/2) * (6/5) ~ 54\63.
--  3 steps = 33 % 32: str 0 fret 3                  (3,571,38.69)
--  6 steps = 17 % 16: str 1 fret -3 | str 0 fret 6  (6,1143,93.30)
-- 16 steps = 19 % 16: str 2 fret -2                 (16,3048,72.48)
-- 20 steps = 5 % 4:   str 2 fret 2                  (20,3810,-53.61)
-- 29 steps = 11 % 8:  str 3 fret 2                  (29,5524,10.63)
-- 33 steps = 23 % 16: str 4 fret -3 | str 3 fret 6  (33,6286,2.970)
-- 37 steps = 3 % 2:   str 4 fret 1                  (37,7048,28.06)
-- 44 steps = 13 % 8:  str 5 fret -1                 (44,8381,-24.32)
-- 51 steps = 7 % 4:   str 6 fret -3                 (51,9714,26.02)
-- 54 steps = 29 % 16: str 6 fret 0                  (54,10286,-10.05)
-- 60 steps = 31 % 16: str 7 fret -3 | str 6 fret 6  (60,11429,-21.78)
-- 63 steps = 2 % 1:   str 7 fret 0

my63 :: MonomeEdo
my63 = MonomeEdo
  { _edo = 63
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,9)
      , _gridHorizontalVector = (7,0)
      }
  }

-- | 0  steps; 1  % 1 ;  string 0 ; fret 0
--   63 steps; 2  % 1 ;  string 6 ; fret 1
--   37 steps; 3  % 2 ;  string 4 ; fret -1
--   20 steps; 5  % 4 ;  string 2 ; fret 0
--   51 steps; 7  % 4 ;  string 6 ; fret -3
--   11 steps; 9  % 8 ;  string 2 ; fret -3
--   29 steps; 11 % 8 ;  string 2 ; fret 3
--   44 steps; 13 % 8 ;  string 5 ; fret -2
--   57 steps; 15 % 8 ;  string 6 ; fret -1
--   6  steps; 17 % 16 ; string 0 ; fret 2
--   16 steps; 19 % 16 ; string 1 ; fret 2
--   25 steps; 21 % 16 ; string 4 ; fret -5
--   33 steps; 23 % 16 ; string 3 ; fret 1
--   41 steps; 25 % 16 ; string 5 ; fret -3
--   48 steps; 27 % 16 ; string 6 ; fret -4
--   54 steps; 29 % 16 ; string 6 ; fret -2
--   60 steps; 31 % 16 ; string 6 ; fret 0

my63_3_10 :: MonomeEdo
my63_3_10 = MonomeEdo
  { _edo = 63
  , _skip = 3
  , _spacing = 10
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,10)
      , _gridHorizontalVector = (6,1)
      }
  }

-- | 0  steps; 1  % 1 ;  string 0 ; fret 0
--   63 steps; 2  % 1 ;  string 3 ; fret 4
--   37 steps; 3  % 2 ;  string 1 ; fret 4
--   20 steps; 5  % 4 ;  string 2 ; fret - 1
--   51 steps; 7  % 4 ;  string 3 ; fret 2
--   11 steps; 9  % 8 ;  string - 1 ; fret 4
--   29 steps; 11 % 8 ;  string - 1 ; fret 7
--   44 steps; 13 % 8 ;  string 2 ; fret 3
--   57 steps; 15 % 8 ;  string 3 ; fret 3
--   6  steps; 17 % 16 ; string 0 ; fret 1
--   16 steps; 19 % 16 ; string - 2 ; fret 7
--   25 steps; 21 % 16 ; string 1 ; fret 2
--   33 steps; 23 % 16 ; string 3 ; fret - 1
--   41 steps; 25 % 16 ; string - 1 ; fret 9
--   48 steps; 27 % 16 ; string 0 ; fret 8
--   54 steps; 29 % 16 ; string 0 ; fret 9
--   60 steps; 31 % 16 ; string 0 ; fret 10

my63_6_13 :: MonomeEdo
my63_6_13 = MonomeEdo
  { _edo = 63
  , _skip = 6
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-6,13)
      , _gridHorizontalVector = (3,4)
      }
  }

-- | This might be guitar-feasible.
-- The layout is miraculous.
--   All primes but 19 lay on frets [0,7].
--   6 steps = 17 % 16 : str 0 fr 2
--  17 steps = 19 % 16 : str 2 fr -3 -- ignore; the rest are a miracle
--  22 steps = 5  % 4  : str 1 fr 3
--  31 steps = 11 % 8  : str 1 fr 6
--  35 steps = 23 % 16 : str 2 fr 3
--  39 steps = 3  % 2  : str 3 fr 0
--  47 steps = 13 % 8  : str 2 fr 7
--  54 steps = 7  % 4  : str 3 fr 5
--  57 steps = 29 % 16 : str 3 fr 6
--  64 steps = 31 % 16 : str 4 fr 4
--  67 steps = 2  % 1  : str 4 fr 5
-- HOWEVER: It's nonetheless difficult, because
--  6/5 : str 2 fr -3
--  9/8 : str 2 fr -5
--  10/9: str -2 fr 8
-- It sounds incredible.
--   Primes 5, 19 and 29 have an absolute error in [6.9, 8.7] cents.
--   All others have it less than 4c.

my67 :: MonomeEdo
my67 = MonomeEdo
  { _edo = 67
  , _skip = 3
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,13)
      , _gridHorizontalVector = (4,5)
      }
  }

-- | 77 steps = 2 % 1: string 5 fret 4
--   62 steps = 7 % 4: string 5 fret -1
--   54 steps = 13 % 8: string 3 fret 5
--   45 steps = 3 % 2: string 3 fret 2
--   35 steps = 11 % 8: string 2 fret 3
--   25 steps = 5 % 4: string 1 fret 4

my77_mod3 :: MonomeEdo
my77_mod3 = MonomeEdo
  { _edo = 77
  , _skip = 3
  , _spacing = 13
  , _octaveStretchInCents = 0.222
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-3,13)
      , _gridHorizontalVector = (5,4)
      }
  }

-- | 77 edo, mod 1, spaced 11
--  3 steps = 33 % 32: string 0 fret  3                   (468,-65.19)
--  7 steps = 17 % 16: string 1 fret -4 | string 0 fret 7 ( 7, 1091, 41.35)
-- 19 steps = 19 % 16: string 2 fret -3 | string 1 fret 8 (19, 2961,-14.09)
-- 25 steps = 5 % 4:   string 2 fret  3                   (25, 3896, 32.96)
-- 35 steps = 11 % 8:  string 3 fret  2                   (35, 5455,-58.63)
-- 40 steps = 23 % 16: string 4 fret -4 | string 3 fret 7 (40, 6234,-48.97)
-- 45 steps = 3 % 2:   string 4 fret  1                   (45, 7013, -6.56)
-- 54 steps = 13 % 8:  string 5 fret -1                   (54, 8416, 10.30)
-- 62 steps = 7 % 4:   string 6 fret -4                   (62, 9662,-25.92)
-- 66 steps = 29 % 16: string 6 fret  0                   (66,10286,-10.05)
-- 73 steps = 31 % 16: string 7 fret -4 | string 6 fret 7 (73,11377,-73.73)
-- 77 steps = 2 % 1:   string 7 fret  0
my77 :: MonomeEdo
my77 = MonomeEdo
  { _edo = 77
  , _skip = 1
  , _spacing = 11
  , _octaveStretchInCents = 0.222
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,11)
      , _gridHorizontalVector = (7,0)
      }
  }

-- | 84 steps = 2 % 1  : string 6 fret 2
--   68 steps = 7 % 4  : string 5 fret 1
--   59 steps = 13 % 8 : string 5 fret -2
--   49 steps = 3 % 2  : string 4 fret -1
--   39 steps = 11 % 8 : string 3 fret 0
--   27 steps = 5 % 4  : string 3 fret -4
my84' :: MonomeEdo
my84' = MonomeEdo
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

my90 :: MonomeEdo
my90 = MonomeEdo
  { _edo = 90
  , _skip = 5
  , _spacing = 17
  , _octaveStretchInCents = -0.542
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-5,17)
      , _gridHorizontalVector = (5,1)
      }
  }

-- | 94 steps; 2  % 1 ;  string 5 ; fret 2
--   55 steps; 3  % 2 ;  string 3 ; fret 1
--   30 steps; 5  % 4 ;  string 1 ; fret 2
--   76 steps; 7  % 4 ;  string 3 ; fret 4
--   16 steps; 9  % 8 ;  string 1 ; fret 0
--   43 steps; 11 % 8 ;  string 4 ; fret - 3
--   66 steps; 13 % 8 ;  string 5 ; fret - 2
--   85 steps; 15 % 8 ;  string 4 ; fret 3
--   8  steps; 17 % 16 ; string 4 ; fret - 8
     -- 2nd best 9\94    string 1 fret 1, 13 cents sharp
--   23 steps; 19 % 16 ; string 1 ; fret 1
--   37 steps; 21 % 16 ; string 1 ; fret 3
--   49 steps; 23 % 16 ; string 0 ; fret 7
     -- 3rd best 51\94   string 3 fret 0, 23 cents sharp
--   61 steps; 25 % 16 ; string 6 ; fret - 5
--   71 steps; 27 % 16 ; string 4 ; fret 1
--   81 steps; 29 % 16 ; string 2 ; fret 7
--   90 steps; 31 % 16 ; string 3 ; fret 6

my94 :: MonomeEdo
my94 = MonomeEdo
  { _edo = 94
  , _skip = 7
  , _spacing = 16
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-7,16)
      , _gridHorizontalVector = (5,2)
      }
  }

-- | 105 steps = 2 % 1  : string 5 fret 2
--   85 steps  = 7 % 4  : string 5 fret 0
--   74 steps  = 13 % 8 : string 2 fret 4
--   61 steps  = 3 % 2  : string 3 fret 1
--   48 steps  = 11 % 8 : string 4 fret -2
--   34 steps  = 5 % 4  : string 2 fret 0
my105 :: MonomeEdo
my105 = MonomeEdo
  { _edo = 105
  , _skip = 10
  , _spacing = 17
  , _octaveStretchInCents = 0.086
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-10,17)
      , _gridHorizontalVector = (5,2)
      }
  }

my109 :: MonomeEdo
my109 = MonomeEdo
  { _edo = 109
  , _skip = 7
  , _spacing = 19
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-7,19)
      , _gridHorizontalVector = (5,2)
      }
  }

-- | 118 steps = 2 % 1  : string 6t fret 1
--   95 steps  = 7 % 4  : string 5 fret 0
--   83 steps  = 13 % 8 : string 5 fret -3
--   69 steps  = 3 % 2  : string 3 fret 3
--   54 steps  = 11 % 8 : string 2 fret 4
--   38 steps  = 5 % 4  : string 2 fret 0
my118 :: MonomeEdo
my118 = MonomeEdo
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
my118_diagonal :: MonomeEdo
my118_diagonal = MonomeEdo
  { _edo = 118
  , _skip = 4
  , _spacing = 23
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-4,23)
      , _gridHorizontalVector = (6,-5)
      }
  }

-- |
--  5 steps = 33 % 32:  string  0 fret  5 (  5,  508,-24.25)
-- 10 steps = 17 % 16:  string  1 fret  0 ( 10, 1017,-32.60)
-- 29 steps = 19 % 16:  string  3 fret -1 ( 29, 2949,-25.97)
-- 38 steps = 5 % 4:    string  4 fret -2 ( 38, 3864,  1.26)
-- 54 steps = 11 % 8:   string  5 fret  4 ( 54, 5492,-21.65)
-- 62 steps = 23 % 16:  string  6 fret  2 ( 62, 6305, 22.34)
-- 69 steps = 3 % 2:    string  7 fret -1 ( 69, 7017, -2.60)
-- 83 steps = 13 % 8:   string  8 fret  3 ( 83, 8441, 35.40)
-- 95 steps = 7 % 4:    string  9 fret  5 ( 95, 9661,-27.24)
-- 101 steps = 29 % 16: string 10 fret  1 (101,10271,-24.58)
-- 113 steps = 31 % 16: string 11 fret  3 (113,11492, 41.16)
-- 118 steps = 2 % 1:   string 12 fret -2
my118_by10 :: MonomeEdo
my118_by10 = MonomeEdo
  { _edo = 118
  , _skip = 1
  , _spacing = 10
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,10)
      , _gridHorizontalVector = (12,-2)
      }
  }

my87 :: MonomeEdo
my87 = MonomeEdo
  { _edo = 87
  , _skip = 2
  , _spacing = 17
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,17)
      , _gridHorizontalVector = (5,1)
      }
  }

-- | Max reach 8 frets, nice 4:5:6:8, great octave
--  7 steps = 17 % 16: str 1 fret -5 | str 0 fret 7 (7,1000,-49.55)
-- 21 steps = 19 % 16: str 2 fret -3 | str 1 fret 9 (21,3000,24.86)
-- 27 steps = 5 % 4:   str 2 fret 3                    (27,3857,-5.994)
-- 39 steps = 11 % 8:  str 3 fret 3                    (39,5571,58.24)
-- 44 steps = 23 % 16: str 4 fret -4 | str 3 fret 8 (44,6286,2.970)
-- 49 steps = 3 % 2:   str 4 fret 1                    (49,7000,-19.55)
-- 59 steps = 13 % 8:  str 5 fret -1                   (59,8429,23.29)
-- 68 steps = 7 % 4:   str 6 fret -4                   (68,9714,26.02)
-- 72 steps = 29 % 16: str 6 fret 0                    (72,10286,-10.05)
-- 80 steps = 31 % 16: str 7 fret -4 | str 6 fret 8 (80,11429,-21.78)
-- 84 steps = 2 % 1:   str 7 fret 0
my84 :: MonomeEdo
my84 = MonomeEdo
  { _edo = 84
  , _skip = 1
  , _spacing = 12
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,12)
      , _gridHorizontalVector = (7,0)
      }
  }

-- | Consistent through the 19-limit,
-- and extremely accurate even through the 29-limit,
-- the positive errors canceling.
-- The maximum possible error for a note is 7.5 cents.
-- 07 steps = 17 % 16: string 1 fret -3  (105,  .04)
-- 20 steps = 19 % 16: string 2 fret -3  (300, 2.48)
-- 26 steps = 5 % 4:   string 2 fret 0   (390, 3.68)
-- 37 steps = 11 % 8:  string 3 fret -1  (555, 3.68)
-- 42 steps = 23 % 16: string 4 fret -5  (630, 1.72)
-- 47 steps = 3 % 2:   string 3 fret 4   (705, 3.04)
-- 56 steps = 13 % 8:  string 4 fret 2   (840, -.52)
-- 65 steps = 7 % 4:   string 5 fret 0   (975, 6.17)
-- 69 steps = 29 % 16: string 5 fret 2  (1035, 5.42)
-- 76 steps = 31 % 16: string 6 fret -1 (1140,-5.03)
-- 80 steps = 2 % 1:   string 6 fret 1

my80_2_13 :: MonomeEdo
my80_2_13 = MonomeEdo
  { _edo = 80
  , _skip = 2
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,13)
      , _gridHorizontalVector = (6,1)
      }
  }

-- | 7 steps = 17 % 16:str 1 fret -3 | str 0 fret 7
-- 20 steps = 19 % 16: str 2 fret 0
-- 26 steps = 5 % 4:   str 3 fret -4
-- 37 steps = 11 % 8:  str 4 fret -3
-- 42 steps = 23 % 16: str 4 fret 2
-- 47 steps = 3 % 2:   str 5 fret -3
-- 56 steps = 13 % 8:  str 6 fret -4
-- 65 steps = 7 % 4:   str 7 fret -5
-- 69 steps = 29 % 16: str 7 fret -1
-- 76 steps = 31 % 16: str 8 fret -5 | str 7 fret 6
-- 80 steps = 2 % 1:   str 8 fret 0

my80_1_10 :: MonomeEdo
my80_1_10 = MonomeEdo
  { _edo = 80
  , _skip = 1
  , _spacing = 10
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,10)
      , _gridHorizontalVector = (8,0)
      }
  }

-- | 80 edo, single-spaced, octave at (7,3)
--  7 steps = 17 % 16 : str 1 fret -4 | str 0 fret 7
-- 20 steps = 19 % 16 : str 2 fret -2
-- 26 steps = 5 % 4   : str 2 fret 4
-- 37 steps = 11 % 8  : str 3 fret 4
-- 42 steps = 23 % 16 : str 4 fret -2
-- 47 steps = 3 % 2   : str 4 fret 3
-- 56 steps = 13 % 8  : str 5 fret 1
-- 65 steps = 7 % 4   : str 6 fret -1
-- 69 steps = 29 % 16 : str 6 fret 3
-- 76 steps = 31 % 16 : str 7 fret -1
-- 80 steps = 2 % 1   : str 7 fret 3

my80 :: MonomeEdo
my80 = my80_1_11

my80_1_11 :: MonomeEdo
my80_1_11 = MonomeEdo
  { _edo = 80
  , _skip = 1
  , _spacing = 11
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,11)
      , _gridHorizontalVector = (7,3)
      }
  }

-- |
-- 7 steps = 17 % 16: str 1 fret -2
--20 steps = 19 % 16: str 2 fret  2
--26 steps = 5 % 4:   str 3 fret -1
--37 steps = 11 % 8:  str 4 fret  1
--42 steps = 23 % 16: str 5 fret -3 | str 4 fret 6
--47 steps = 3 % 2:   str 5 fret  2
--56 steps = 13 % 8:  str 6 fret  2
--65 steps = 7 % 4:   str 7 fret  2
--69 steps = 29 % 16: str 8 fret -3 | str 7 fret 6
--76 steps = 31 % 16: str 8 fret  4
--80 steps = 2 % 1:   str 9 fret -1

my80_1_9 :: MonomeEdo
my80_1_9 = MonomeEdo
  { _edo = 80
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-1,9)
      , _gridHorizontalVector = (9,-1)
      }
  }

-- | Horizontally wider, vertically narrower.
-- 07 steps = 17 % 16 : string 1 fret -1   (105,  .04)
-- 20 steps = 19 % 16 : string 2 fret  1   (300, 2.48)
-- 26 steps = 5 % 4   : string 2 fret  4   (390, 3.68)
-- 37 steps = 11 % 8  : string 3 fret  5   (555, 3.68)
-- 42 steps = 23 % 16 : string 4 fret  3   (630, 1.72)
-- 47 steps = 3 % 2   : string 5 fret  1   (705, 3.04)
-- 56 steps = 13 % 8  : string 6 fret  1   (840, -.52)
-- 65 steps = 7 % 4   : string 7 fret  1   (975, 6.17)
-- 69 steps = 29 % 16 : string 7 fret  3  (1035, 5.42)
-- 76 steps = 31 % 16 : string 8 fret  2  (1140,-5.03)
-- 80 steps = 2 % 1   : string 8 fret  4
my80_thanos :: MonomeEdo
my80_thanos = MonomeEdo
  { _edo = 80
  , _skip = 2
  , _spacing = 9
  , _octaveStretchInCents = 0
  , _gridVectors = Just $ GridVectorPair
      { _gridVerticalVector = (-2,9)
      , _gridHorizontalVector = (8,4)
      }
  }

-- | Unfortunately, in 48-edo the second-best 5:4
-- plays better with 7:4 and 13:8,
-- and it's located at  string 2 fret -3
--
-- 48 steps = 2 % 1   : string 4 fret 2
-- 28 steps = 3 % 2   : string 2 fret 3
-- 15 steps = 5 % 4   : string 1 fret 2
-- 39 steps = 7 % 4   : string 3 fret 3
-- 22 steps = 11 % 8  : string 2 fret 0
-- 34 steps = 13 % 8  : string 2 fret 6
-- 4 steps = 17 % 16  : string 0 fret 2
-- 12 steps = 19 % 16 : string 0 fret 6
-- 25 steps = 23 % 16 : string 1 fret 7
-- 41 steps = 29 % 16 : string 3 fret 4
-- 46 steps = 31 % 16 : string 4 fret 1

my48 :: MonomeEdo
my48 = MonomeEdo
  { _edo = 48
  , _skip = 1
  , _spacing = 11
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my46 :: MonomeEdo
my46 = MonomeEdo
  { _edo = 46
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my46_1_7 :: MonomeEdo
my46_1_7 = MonomeEdo
  { _edo = 46
  , _skip = 1
  , _spacing = 7
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }


-- | 4 \ 46 = 17 % 16 : str 0 fret 2
--  11 \ 46 = 19 % 16 : str 1 fret -2
--  15 \ 46 = 5 % 4   : str 1 fret 0
--  21 \ 46 = 11 % 8  : str 1 fret 3
--  24 \ 46 = 23 % 16 : str 2 fret -3
--  27 \ 46 = 3 % 2   : str 1 fret 6
--                      str 3 fret -9
--  32 \ 46 = 13 % 8  : str 2 fret 1
--  37 \ 46 = 7 % 4   : str 3 fret -4
--                      str 1 fret 11
--  39 \ 46 = 29 % 16 : str 3 fret -3
--  44 \ 46 = 31 % 16 : str 2 fret 7
--  46 \ 46 = 2 % 1   : str 4 fret -7
--                      str 2 fret 8
my46_2_15 :: MonomeEdo
my46_2_15 = MonomeEdo
  { _edo = 46
  , _skip = 2
  , _spacing = 15
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

-- | 41 steps = 2 % 1  : string 3 fret 1
--   33 steps = 7 % 4  : string 3 fret -3
--   29 steps = 13 % 8 : string 3 fret -5
--   24 steps = 3 % 2  : string 2 fret -1
--   19 steps = 11 % 8 : string 1 fret 3
--   13 steps = 5 % 4  : string 1 fret 0
myKite :: MonomeEdo
myKite = MonomeEdo
  { _edo = 41
  , _skip = 2
  , _spacing = 13
  , _octaveStretchInCents = 0
  , _gridVectors =  Just $ GridVectorPair
      { _gridVerticalVector = ( -2, 13 )
      , _gridHorizontalVector = (3,1)
      }
  }

my41_2_11 :: MonomeEdo
my41_2_11 = MonomeEdo
  { _edo = 41
  , _skip = 2
  , _spacing = 11
  , _octaveStretchInCents = 0
  , _gridVectors =  Just $ GridVectorPair
      { _gridVerticalVector = ( -2, 11 )
      , _gridHorizontalVector = (3,4)
      }
  }

my41 :: MonomeEdo
my41 = MonomeEdo
  { _edo = 41
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my37 :: MonomeEdo
my37 = MonomeEdo
  { _edo = 37
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = 0 -- haven't checked
  , _gridVectors = Nothing
  }

my34 :: MonomeEdo
my34 = MonomeEdo
  { _edo = 34
  , _skip = 1
  , _spacing = 7
  , _octaveStretchInCents = -1.1 -- 5-limit optimal.
    -- 13-limit optimal is -2.64, but uses the 34d mapping.
    -- 7-limit optimal, patent val, is +0.58.
  , _gridVectors = Nothing
  }

-- | 3 steps = 17 % 16 : string -1 fret 6
--   8 steps = 19 % 16 : string 0 fret 4
--  11 steps = 5  % 4  : string 1 fret 1
--  16 steps = 11 % 8  : string 2 fret -1
--  18 steps = 23 % 16 : string 2 fret 0
--  20 steps = 3  % 2  : string 2 fret 1
--  24 steps = 13 % 8  : string 2 fret 3
--  27 steps = 7  % 4  : string 3 fret 0
--  29 steps = 29 % 16 : string 3 fret 1
--  32 steps = 31 % 16 : string 4 fret -2
--  34 steps = 2  % 1  : string 4 fret -1

my34_thanos :: MonomeEdo
my34_thanos = MonomeEdo
  { _edo = 34
  , _skip = 2
  , _spacing = 9
  , _octaveStretchInCents = -1.1 -- 5-limit optimal.
    -- 13-limit optimal is -2.64, but uses the 34d mapping.
    -- 7-limit optimal, patent val, is +0.58.
  , _gridVectors =  Just $ GridVectorPair
      { _gridVerticalVector = (-2, 9)
      , _gridHorizontalVector = (4,-1)
      }
  }

my31 :: MonomeEdo
my31 = MonomeEdo
  { _edo = 31
  , _skip = 1
  , _spacing = 6
  , _octaveStretchInCents = 0.5
  , _gridVectors = Nothing
  }

my31_1_9 :: MonomeEdo
my31_1_9 = MonomeEdo
  { _edo = 31
  , _skip = 1
  , _spacing = 9
  , _octaveStretchInCents = 0.5
  , _gridVectors = Nothing
  }

my31_1_11 :: MonomeEdo
my31_1_11 = MonomeEdo
  { _edo = 31
  , _skip = 1
  , _spacing = 11
  , _octaveStretchInCents = 0.5
  , _gridVectors = Nothing
  }

my31_2_9 :: MonomeEdo
my31_2_9 = MonomeEdo
  { _edo = 31
  , _skip = 2
  , _spacing = 9
  , _octaveStretchInCents = 0.5
  , _gridVectors =  Just $ GridVectorPair
      { _gridVerticalVector = (-2, 9)
      , _gridHorizontalVector = (3, 2)
      }
  }

my31_2_11 :: MonomeEdo
my31_2_11 = MonomeEdo
  { _edo = 31
  , _skip = 2
  , _spacing = 11
  , _octaveStretchInCents = 0.5
  , _gridVectors =  Just $ GridVectorPair
      { _gridVerticalVector = (-2, 11)
      , _gridHorizontalVector = (3, -1)
      }
  }

my27 :: MonomeEdo
my27 = MonomeEdo
  { _edo = 27
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = -4.5 -- 11-limit optimal
                         -- -3.9 -- 13-limit optimal
  , _gridVectors = Nothing
  }

my26 :: MonomeEdo
my26 = MonomeEdo
  { _edo = 26
  , _skip = 1
  , _spacing = 8
  , _octaveStretchInCents = 2
    -- 1.5 is 13-limit optimal skipping 5
    -- 2.5 -- both 11- and 13-limit optimal
    -- 7-limit optimal is 3.3c
  , _gridVectors = Nothing
  }

my24 :: MonomeEdo
my24 = MonomeEdo
  { _edo = 24
  , _skip = 1
  , _spacing = 7
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my23 :: MonomeEdo
my23 = MonomeEdo
  { _edo = 23
  , _skip = 1
  , _spacing = 7
  , _octaveStretchInCents = 7.5
    -- 9 cents is 13-limit optimal for 23-deff,
    -- which plays each of harmonics 7,11 and 13 1 step flat.
    -- 7.5 cents is 13-limit optimal for 23-de.
    -- 8 cents is 5-limit optimal for 23.
  , _gridVectors = Nothing
  }

my22 :: MonomeEdo
my22 = MonomeEdo
  { _edo = 22
  , _skip = 1
  , _spacing = 7
  , _octaveStretchInCents = -1.1
    -- -1.1 -- 11-limit optimal
    -- -1.8 -- 7-limit optimal
  , _gridVectors = Nothing
  }

my20 :: MonomeEdo
my20 = MonomeEdo
  { _edo = 20
  , _skip = 1
  , _spacing = 6
  , _octaveStretchInCents = -10 --optimal for 11-limit without 7.
      -- 11-limit and 13-limit are similar but a little bigger.
  , _gridVectors = Nothing
  }

my19 :: MonomeEdo
my19 = MonomeEdo
  { _edo = 19
  , _skip = 1
  , _spacing = 6
  , _octaveStretchInCents = 3
      -- 2.58 -- 5-limit optimal
      -- 3.85 -- 7-limit optimal
  , _gridVectors = Nothing
  }

my17 :: MonomeEdo
my17 = MonomeEdo
  { _edo = 17
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents = -- Optimal for 2.3.11.13 is -2 cents.
    -2.5                    -- If you include harmonic 7, it's -3 cents.
  , _gridVectors = Nothing
  }

my16 :: MonomeEdo
my16 = MonomeEdo
  { _edo = 16
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents =
      7.3    -- 5-limit optimal
  , _gridVectors = Nothing
  }

my15 :: MonomeEdo
my15 = MonomeEdo
  { _edo = 15
  , _skip = 1
  , _spacing = 5
  , _octaveStretchInCents = -5
    -- -20 -- 13-limit optimal, but *so* flat
  , _gridVectors = Nothing
  }

my13 :: MonomeEdo
my13 = MonomeEdo
  { _edo = 13
  , _skip = 1
  , _spacing = 4
  , _octaveStretchInCents = 0
  , _gridVectors = Nothing
  }

my12 :: MonomeEdo
my12 = MonomeEdo
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
