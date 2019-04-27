module Vivid.Scale where

import Data.List ((!!))


-- | = scales

-- | All the scales in 12 tone equal temperament with:
-- (a) at least 6 notes
-- (b) no interval is greater than a minor third
-- (c) no three consecutive pitches occupy three consecutive half steps
-- Surprisingly, there are only 33!
nice12etScales = concat [ symmetricScales
                        , diatonicScales
                        , harmonicScales
                        , antiharmonicScales
                        , melodicScales ]

symmetricScales = [ -- ^ symmetric scales
    (["dim"],  [0,2,3,5,6,8,9,11]) -- diminished up
  , (["dimd"], [0,1,3,4,6,7,9,10]) -- diminished down
  , (["aug"],  [0,3,4,7,8,11]) -- augmented up
  , (["augd"], [0,1,4,5,8,9]) -- aug down
  , (["hol"],  [0,2,4,6,8,10::Float]) -- whole tone
  ]
[dim,dimd,aug,augd,hol] =
  map snd symmetricScales

diatonicScales = [ -- ^ diatonic family of scales
    (["maj"], [0,2,4,5,7,9,11]) -- major = ionian
  , (["dor"], [0,2,3,5,7,9,10]) -- dorian
  , (["phr"], [0,1,3,5,7,8,10]) -- phrygian
  , (["lyd"], [0,2,4,6,7,9,11]) -- lydian
  , (["mix"], [0,2,4,5,7,9,10]) -- mixolydian
  , (["aol"], [0,2,3,5,7,8,10]) -- aeolian
  , (["loc"], [0,1,3,5,6,8,10::Float]) -- locrian
  ]
[maj,dor,phr,lyd,mix,aol,loc] =
  map snd diatonicScales

harmonicScales = [ -- ^ harmonic minor family
    (["maj5"],  [0,2,4,5,8,9,11]) -- major #5
  , (["dor4"],  [0,2,3,6,7,9,10]) -- dorian #4
  , (["phr3"],  [0,1,4,5,7,8,10]) -- phrygian #3
  , (["lyd2"],  [0,3,4,6,7,9,11]) -- lydian #2
  , (["loc47"], [0,1,3,4,6,8,9]) -- locrian b4b7
  , (["aol7"],  [0,2,3,5,7,8,11]) -- aeolian #7
  , (["loc6"],  [0,1,3,5,6,9,10::Float]) -- locrian #6
  ]
[maj5,dor4,phr3,lyd2,loc47,aol7,loc6] =
  map snd harmonicScales

antiharmonicScales = [ -- ^ harmonic minor family played backward,
                       -- i.e. reflecting highs for lows in pitch space.
    (["maj6"],  [0,2,4,5,7,8,11]) -- major b6
  , (["dor5"],  [0,2,3,5,6,9,10]) -- dorian b5
  , (["phr4"],  [0,1,3,4,7,8,10]) -- phrygian b4
  , (["lyd3"],  [0,2,3,6,7,9,11]) -- lydian b3
  , (["mix2"],  [0,1,4,5,7,9,10]) -- mixolydian b2
  , (["lyd25"], [0,3,4,6,8,9,11]) -- lydian #2#5
  , (["loc7"],  [0,1,3,5,6,8,9::Float]) -- locrian b7
  ]
[maj6,dor5,phr4,lyd3,mix2,lyd25,loc7] =
  map snd antiharmonicScales

melodicScales = [ -- ^ ("ascending") melodic minor family.
                  -- Most of these scales have two "best" (simplest) names.
    (["maj3","dor7"], [0,2,3,5,7,9,11]) -- major b3 = dorian #7
  , (["dor2","phr6"], [0,1,3,5,7,9,10]) -- dorian b2 = phrygian #6
  , (["lyd5"],        [0,2,4,6,8,9,11]) -- lydian #5
  , (["lyd7","mix4"], [0,2,4,6,7,9,10]) -- lydian b7 = mixolydian #4
  , (["mix6","aol3"], [0,2,4,5,7,8,10]) -- mixolydian b6 = aeolian #3
  , (["aol5","loc2"], [0,2,3,5,6,8,10]) -- aeolian b5 = locrian #2
  , (["loc4"],        [0,1,3,4,6,8,10::Float]) -- locrian b4
  ]
maj3 = snd $ melodicScales !! 0
dor7 = snd $ melodicScales !! 0
dor2 = snd $ melodicScales !! 1
phr6 = snd $ melodicScales !! 1
lyd5 = snd $ melodicScales !! 2
lyd7 = snd $ melodicScales !! 3
mix4 = snd $ melodicScales !! 3
mix6 = snd $ melodicScales !! 4
aol3 = snd $ melodicScales !! 4
aol5 = snd $ melodicScales !! 5
loc2 = snd $ melodicScales !! 5
loc4 = snd $ melodicScales !! 6
