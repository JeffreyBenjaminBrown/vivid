{-# LANGUAGE ScopedTypeVariables
, DataKinds
, LambdaCase
#-}

module Montevideo.EarTrain where

import qualified Data.List as L

import Vivid
import Montevideo.Util
import Montevideo.EarTrain.Audio
import Montevideo.EarTrain.Types


-- | = User-facing quiz functions.

-- | quizzes from subsets drawn uniformly from a chromatic range
earTrainChromatic :: Edo -> Int -> Int -> IO ()
earTrainChromatic edo numberOfFreqs range =
  runEarTests $
  pickChromaticTest edo numberOfFreqs range

-- | like earTrainChromatic, but no 3 consecutive chromatically adjacent tones
earTrain3ClusterFreeChromatic :: Edo -> Int -> Int -> IO ()
earTrain3ClusterFreeChromatic edo numberOfFreqs range =
  runEarTests $
  pick3ClusterFreeTest edo numberOfFreqs range

-- | e.g. `earTrainFromScale 12 3 [0,2,4,6,8,10]` will quiz you on
-- 3 note subsets of the whole tone scale in 12-edo.
earTrainFromScale :: Edo -> Int -> [Float] -> IO ()
earTrainFromScale edo numberOfFreqs scale =
  runEarTests $
  pickTestFromScale edo scale numberOfFreqs

-- | quizzes from a list of chords
earTrainFromChordList :: Edo -> [[Float]] -> IO ()
earTrainFromChordList edo chords =
  runEarTests $
  pickTestFromChordList edo chords

-- | quizzes from major-minor polychords
earTrainPolyChords :: IO ()
earTrainPolyChords =
  runEarTests $
  pickPolyChordTest 12 [[0,3,7],[0,4,7],[0,3,6],[0,4,8]]

-- | quiz two chords in serial
earTrainChromatic2Serial :: Edo -> Int -> Int -> IO ()
earTrainChromatic2Serial edo numberOfFreqs range =
  runEarTests $
  pickChromatic2SerialTest edo numberOfFreqs range


-- | = The top IO workhorse

runEarTests :: IO Test -> IO ()
runEarTests pickTest = pickTest >>= runTest where
  runTest :: Test -> IO ()
  runTest test@(playFreqsMethod, showFreqsMethod) = do
    showChoices
    playFreqsMethod
    getChar >>= \case
      'a' -> putStrLn "nother sound!\nBTW, the last sound was:"
             >> showFreqsMethod
             >> runEarTests pickTest
      's' -> putStrLn "how what was played"
             >> showFreqsMethod
             >> runTest test
      'q' -> putStrLn "uit" >> return ()
      _   -> putStrLn "replay" >> runTest test

showChoices :: IO ()
showChoices = putStrLn $ "\nPlease press a key:\n"
                      ++ "  s: (s)how what was played\n"
                      ++ "  a: play (a)nother chord\n"
                      ++ "  q: (q)uit,\n"
                      ++ "  any other key: replay the sound."


-- | = Test-producing functions (used by earTrainChromatic)

pickTestFromScale :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
  => Edo -> [a] -> Int -> IO Test
pickTestFromScale edo =
  pickTestFromPitchSet edo L.delete

-- | play two chords in serial
pickChromatic2SerialTest :: Edo -> Int -> Int -> IO Test
pickChromatic2SerialTest edo numFreqs range = do
  (q,a) <- pickChromaticTest edo numFreqs range
  (q',a') <- pickChromaticTest edo numFreqs range
  return (q >> q', a >> a')

pick3ClusterFreeTest :: Edo -> Int -> Int -> IO Test
pick3ClusterFreeTest edo = _pickChromaticTest edo x
  where x :: Float -> [Float] -> [Float]
        x = no3Clusters

pickChromaticTest :: Edo -> Int -> Int -> IO Test
pickChromaticTest edo = _pickChromaticTest edo x
  where x :: Float -> [Float] -> [Float]
        x = L.delete

-- | builds and picks Tests from a list of chords.
pickTestFromChordList :: Edo -> [[Float]] -> IO Test
pickTestFromChordList edo chords = do
  transpose <- pick [0..11]
  freqs <- pick chords
  let playSound = playFreqs $ fmap (edoValToFreq edo 220)
                  $ fmap ((+) transpose) freqs :: IO ()
      showFreqs = putStrLn $ "  transpose: " ++ show transpose
                           ++ "\n  chord: " ++ show freqs
  return (playSound, showFreqs)

-- | picks two chords, plays them in different octaves
pickPolyChordTest :: Edo -> [[Float]] -> IO Test
pickPolyChordTest edo chords = do
  transposeLo  <- pick [0..11]
  transposeHi <- pick [0..11]
  freqsLo <- pick chords
  freqsHi <- pick chords
  let allTransposedFreqs = fmap ((+) transposeLo)        freqsLo
                        ++ fmap ((+) $ transposeHi + 17) freqsHi
      least = minimum allTransposedFreqs
      playSound = playFreqs $ fmap (edoValToFreq edo 110) allTransposedFreqs
      showFreqs = putStrLn $ show $ ((+) (- least)) <$> allTransposedFreqs
  return (playSound, showFreqs)

_pickChromaticTest
  :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
  => Edo -> (a -> [a] -> [a]) -> Int -> Int -> IO Test
_pickChromaticTest edo howToDelete numberOfFreqs range =
  pickTestFromPitchSet edo howToDelete (map fromIntegral [0..range]) numberOfFreqs

pickTestFromPitchSet
  :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
  => Edo
  -> (a -> [a] -> [a]) -- ^ How (and even whether)
                       -- to delete after selecting a ptch
  -> [a] -- ^ The pitch set
  -> Int -- ^ How many pitches to select
  -> IO Test
pickTestFromPitchSet edo howToDelete pitches numberOfFreqs = do
  freqs <- L.sort <$>
    pickSome' howToDelete numberOfFreqs pitches -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      showFreqs :: IO () =
        putStrLn $ "  bass: " ++ show bass
        ++ " semitones above A (220 Hz)\n"
        ++ "  chord: " ++ show normFreqs
        ++ " relative to the bass"
      playSound :: IO () =
        playFreqs $ fmap (edoValToFreq edo 220) freqs
  return (playSound, showFreqs)
