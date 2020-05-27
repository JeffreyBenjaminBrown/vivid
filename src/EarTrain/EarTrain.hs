{-# LANGUAGE ScopedTypeVariables
, DataKinds
, LambdaCase
#-}

module EarTrain.EarTrain where

import qualified Data.List as L

import Vivid
import Util
import EarTrain.Audio


type PlayQuestion = IO () -- ^ make a sound, for the user to identify
type ShowAnswer = IO () -- ^ display something, e.g. "it was a major chord"
type Test = (PlayQuestion, ShowAnswer)


-- | = User-facing quiz functions.

-- | quizzes from subsets drawn uniformly from a chromatic range
earTrainChromatic :: Int -> Int -> IO ()
earTrainChromatic numberOfFreqs range =
  runEarTests $ pickChromaticTest numberOfFreqs range

-- | like earTrainChromatic, but no 3 consecutive chromatically adjacent tones
earTrain3ClusterFreeChromatic :: Int -> Int -> IO ()
earTrain3ClusterFreeChromatic numberOfFreqs range =
  runEarTests $ pick3ClusterFreeTest numberOfFreqs range

-- | e.g. `earTrainFromScale 3 [0,2,4,6,8,10]` will quiz you on
-- 3 note subsets of the whole tone scale.
earTrainFromScale :: Int -> [Float] -> IO ()
earTrainFromScale numberOfFreqs scale =
  runEarTests $ pickTestFromScale scale numberOfFreqs

-- | quizzes from a list of chords
earTrainFromChordList :: [[Float]] -> IO ()
earTrainFromChordList chords =
  runEarTests $ pickTestFromChordList chords

-- | quizzes from major-minor polychords
earTrainPolyChords :: IO ()
earTrainPolyChords =
  runEarTests $ pickPolyChordTest [[0,3,7],[0,4,7],[0,3,6],[0,4,8]]

-- | quiz two chords in serial
earTrainChromatic2Serial :: Int -> Int -> IO ()
earTrainChromatic2Serial numberOfFreqs range =
  runEarTests $ pickChromatic2SerialTest numberOfFreqs range


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
  => [a] -> Int -> IO Test
pickTestFromScale = pickTestFromPitchSet L.delete

-- | play two chords in serial
pickChromatic2SerialTest :: Int -> Int -> IO Test
pickChromatic2SerialTest numFreqs range = do
  (q,a) <- pickChromaticTest numFreqs range
  (q',a') <- pickChromaticTest numFreqs range
  return (q >> q', a >> a')

pick3ClusterFreeTest :: Int -> Int -> IO Test
pick3ClusterFreeTest = _pickChromaticTest x
  where x :: Float -> [Float] -> [Float]
        x = no3Clusters

pickChromaticTest :: Int -> Int -> IO Test
pickChromaticTest = _pickChromaticTest x
  where x :: Float -> [Float] -> [Float]
        x = L.delete

-- | builds and picks Tests from a list of chords.
pickTestFromChordList :: [[Float]] -> IO Test
pickTestFromChordList chords = do
  transpose <- pick [0..11]
  freqs <- pick chords
  let playSound = playFreqs $ fmap (et12toFreq 220)
                  $ fmap ((+) transpose) freqs :: IO ()
      showFreqs = putStrLn $ "  transpose: " ++ show transpose
                           ++ "\n  chord: " ++ show freqs
  return (playSound, showFreqs)

-- | picks two chords, plays them in different octaves
pickPolyChordTest :: [[Float]] -> IO Test
pickPolyChordTest chords = do
  transposeLo  <- pick [0..11]
  transposeHi <- pick [0..11]
  freqsLo <- pick chords
  freqsHi <- pick chords
  let allTransposedFreqs = fmap ((+) transposeLo)        freqsLo
                        ++ fmap ((+) $ transposeHi + 17) freqsHi
      least = minimum allTransposedFreqs
      playSound = playFreqs $ fmap (et12toFreq 110) allTransposedFreqs
      showFreqs = putStrLn $ show $ ((+) (- least)) <$> allTransposedFreqs
  return (playSound, showFreqs)

_pickChromaticTest :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
                   => (a -> [a] -> [a]) -> Int -> Int -> IO Test
_pickChromaticTest howToDelete numberOfFreqs range =
  pickTestFromPitchSet howToDelete (map fromIntegral [0..range]) numberOfFreqs

pickTestFromPitchSet
  :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
  => (a -> [a] -> [a]) -> [a] -> Int -> IO Test
pickTestFromPitchSet howToDelete range numberOfFreqs = do
  freqs <- L.sort <$>
    pickSome' howToDelete numberOfFreqs range -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      showFreqs :: IO () =
        putStrLn $ "  bass: " ++ show bass
        ++ " semitones above A (220 Hz)\n"
        ++ "  chord: " ++ show normFreqs
        ++ " relative to the bass"
      playSound :: IO () =
        playFreqs $ fmap (et12toFreq 220) freqs
  return (playSound, showFreqs)
