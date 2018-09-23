{-# LANGUAGE ScopedTypeVariables
, DataKinds
, LambdaCase
#-}

module Vivid.Jbb.Scale where

import Data.Fixed (mod',div')
import Data.Map as M
import Data.List as L
import Data.Set as S

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Util (pickSome)


earTrain :: Int -> IO () -- 3 kinds of IO: randomness, sound and text
earTrain numberOfFreqs = do
  freqs <- L.sort <$> pickSome numberOfFreqs [0..23] -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      theSound = playFreqs $ fmap (et12toFreq 220) freqs :: IO ()
      soundAndText :: IO () -> IO ()
      soundAndText sound = do -- 2 kinds of IO: sound(out) and text(in & out)
        putStrLn $ "\nPlease press a key:\n" ++
          "(s)how, (a)nother, (q)uit, or (anything else) replay the sound."
        sound
        getChar >>= \case 'a' -> putStrLn "nother" >> test numberOfFreqs
                          's' -> do putStrLn $ "how\n" ++ show bass
                                      ++ "\n" ++ show normFreqs
                                    soundAndText sound
                          'q' -> putStrLn "uit" >> return ()
                          otherwise -> putStrLn "\nreplay"
                                       >> soundAndText sound
  soundAndText theSound

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.2 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait 1
  mapM_ free synths

et12toFreq :: Floating a => a -> a -> a
et12toFreq baseFreq p = 2**(p/12) * baseFreq


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
    (["dim_s"],  [0,2,3,5,6,8,9,11]) -- diminished up
  , (["dimd_s"], [0,1,3,4,6,7,9,10]) -- diminished down
  , (["aug_s"],  [0,3,4,7,8,11]) -- augmented up
  , (["augd_s"], [0,1,4,5,8,9]) -- aug down
  , (["hol_s"],  [0,2,4,6,8,10]) -- whole tone
  ]

diatonicScales = [ -- ^ diatonic family of scales
    (["maj_s"], [0,2,4,5,7,9,11]) -- major = ionian
  , (["dor_s"], [0,2,3,5,7,9,10]) -- dorian
  , (["phr_s"], [0,1,3,5,7,8,10]) -- phrygian
  , (["lyd_s"], [0,2,4,6,7,9,11]) -- lydian
  , (["mix_s"], [0,2,4,5,7,9,10]) -- mixolydian
  , (["aol_s"], [0,2,3,5,7,8,10]) -- aeolian
  , (["loc_s"], [0,1,3,5,6,8,10]) -- locrian
  ]

harmonicScales = [ -- ^ harmonic minor family
    (["maj5_s"], [0,2,4,5,8,9,11]) -- major #5
  , (["dor4_s"], [0,2,3,6,7,9,10]) -- dorian #4
  , (["phr3_s"], [0,1,4,5,7,8,10]) -- phrygian #3
  , (["lyd2_s"], [0,3,4,6,7,9,11]) -- lydian #2
  , (["loc47_s"], [0,1,3,4,6,8,9]) -- locrian b4b7
  , (["aol7_s"], [0,2,3,5,7,8,11]) -- aeolian #7
  , (["loc6_s"], [0,1,3,5,6,9,10]) -- locrian #6
  ]

antiharmonicScales = [ -- ^ harmonic minor family played backward,
                       -- i.e. reflecting highs for lows in pitch space.
    (["maj6_s"], [0,2,4,5,7,8,11]) -- major b6
  , (["dor5_s"], [0,2,3,5,6,9,10]) -- dorian b5
  , (["phr4_s"], [0,1,3,4,7,8,10]) -- phrygian b4
  , (["lyd3_s"], [0,2,3,6,7,9,11]) -- lydian b3
  , (["mix2_s"], [0,1,4,5,7,9,10]) -- mixolydian b2
  , (["lyd25_s"], [0,3,4,6,8,9,11]) -- lydian #2#5
  , (["loc7_s"], [0,1,3,5,6,8,9]) -- locrian b7
  ]

melodicScales = [ -- ^ ("ascending") melodic minor family.
                  -- Most of these scales have two "best" (simplest) names.
    (["maj3_s","dor7_s"], [0,2,3,5,7,9,11]) -- major b3 = dorian #7
  , (["dor2_s","phr6_s"], [0,1,3,5,7,9,10]) -- dorian b2 = phrygian #6
  , (["lyd5_s"],          [0,2,4,6,8,9,11]) -- lydian #5
  , (["lyd7_s","mix4_s"], [0,2,4,6,7,9,10]) -- lydian b7 = mixolydian #4
  , (["mix6_s","aol3_s"], [0,2,4,5,7,8,10]) -- mixolydian b6 = aeolian #3
  , (["aol5_s","loc2_s"], [0,2,3,5,6,8,10]) -- aeolian b5 = locrian #2
  , (["loc4_s"],          [0,1,3,4,6,8,10]) -- locrian b4
  ]
