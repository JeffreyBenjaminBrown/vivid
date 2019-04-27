-- | == Table of contents
-- (1) How to run the ear training functions here.
-- (2) A study plan: the Victor Burges ear-training program
-- (3) Using Vivid.EarTrain for other stuff: perfect pitch,
--     perfect rhythm, microtonal harmony, distinguishing timbres ...
-- (4) The code


-- | = How to run the ear training functions here.
--
-- (0) You'll need Stack and SuperCollider installed.
--     (I believe using Stack is both easier and safer than using a global
--     Haskell installation. However, if you prefer the latter, Tom Murphy
--     figured out how to do that once, and describes it here:
--     https://we.lurk.org/hyperkitty/list/haskell-art@we.lurk.org/thread/CQU74SOXJTKH3D3MGKVUSVX57Z6PHUYF/
--     starting at the phrase "I'm using ghc+cabal, not stack".)
--
-- (1) Clone this branch of this repo -- for instance, by running:
--     git clone -b jbb-update https://github.com/JeffreyBenjaminBrown/vivid/
--
-- (2) From Bash, visit the repo's root folder -- probably by running:
--     cd vivid
--
-- (3) Start SuperCollider by running:
--     bash sc-start.sh
--     (Or start SuperCollider in whatever other way you prefer.)
--
-- (4) Start GHCI by running:
--     stack ghci
--
-- (5) Try one of these
--     (a) earTrainChromatic n k, where:
--         n is the number of notes you want to be quizzed on at a time.
--         r is the size (measured in halfsteps) of the range of pitches
--           you would like to be quizzed on.
--     (b) earTrainFromChordList as, where `as :: [[Float]]` is a list
--         of chords (in 12 tone equal temperament) you want to be quizzed on.
--         For instance, `earTrainFromChordList [[0,3,7],[0,4,7]] would quiz
--         you on the difference between major and minor triads.
--     A workflow I like is to start with earTrainChromatic, and when
--     something confuses me, I'll use earTrainFromChordList to focus on
--     the alternatives I was unable to distinguish.
--
-- (5) Follow the on-screen prompts.


-- | = A study plan: the Victor Burges ear-training program
--
-- If you understand how to run the program, but you're not sure what to
-- do with it, or it seems too difficult, this comment explains how to
-- start from zero and build up in small incremental steps.
--
-- When I was in high school, I read an ad in Guitar Magazine and ended up
-- (after saving for months) buying the ear training cassette tapes
-- by Victor Burges.
-- They worked: After two months of training maybe 20 minutes a day,
-- I could hear a new song on the smooth jazz radio sation in
-- Guatemala City and know how to play the melody, and sometimes the chords.
--
-- If I recall correctly, Burges first taught perfect fifths.
-- First he would assign homework: You had to find a piano (or something),
-- and play C and G, and sing them. (Or hum, or whistle; whatever.)
-- Then sing C# and G#, then D and A, etc.,
-- all the way up and down an octave. Next do the same thing, only
-- singing the higher note first. Let's call that whole routine
-- "singing an octave of the interval". (Just like a bodybuilder,
-- it's good to mix up the order of operations, so maybe sometimes start
-- singing descending intervals instead of ascending ones.)
--
-- Next, Burges would test whether you had absorbed the interval:
-- He would play a note and ask you to sing a fifth above or below.
-- This part of the Burges tapes was kind of a waste of money, because
-- it's something you can do yourself: Just play a note at random,
-- try to sing a fifth above or below, then play the "answer" and see if
-- you were right. Let's call that test "producing the interval".
--
-- If you can't yet pass the "producing the interval" test, you need to
-- sing another octave of the interval. Cycle through those two steps
-- until you pass the second one. (Don't burn out! After 20 minutes, if
-- you feel drained, you deserve at least an hour's rest. Sensory
-- expansion is weird; holding many brief study periods is
-- surprisingly powerful, and cramming is surprisingly ineffective.)
--
-- Once you've passed the "producing the interval" test, you know perfect
-- fifths! Next, do the same thing to learn fourths: Sing an
-- octave of fourths (ascending and descending), then test yourself at
-- producing fourths.
--
-- Now that you can produce two intervals, you need to learn to
-- distinguish them. (Here is where Vivid.EarTrain becomes useful.)
-- Burge's next step was to ask you to distinguish between fourths and fifths.
-- You can have this software do that, by running
-- `earTrainFromChordList [[0,7],[0,5]]`
--
-- If I recall correctly, the rest of the Burges program was:
--
-- add the major third (the interval [0,4]:
--   (1) sing an octave of major thirds
--   (2) test yourself at producing major thirds
--   (3) `earTrainFromChordList [[0,7],[0,5],[0,4]]`
--
-- add the minor third (the interval [0,3]):
--   (1) sing an octave of minor thirds (the interval [0,3])
--   (2) test yourself at producing minor thirds
--   (3) `earTrainFromChordList [[0,7],[0,5],[0,4],[0,3]]`
--
-- (the same drill applies to every new interval:
--   sing an octave, test producing it, then add it to the list.)
--
-- add the major sixth = [0,9]
-- add the minor sixth = [0,8]
-- add the minor seventh = [0,10]
-- add the major seventh = [0,11]
-- add the major second = [0,2]
-- add the minor second = [0,1]
-- add the tritone = diminished fifth = augmented fourth = [0,6]
--
-- Now you know all the intervals! Those are roughly ordered by increasing
-- difficulty.
--
-- An advantage of Vivid.EarTrain over the cassettes of my youth is that
-- if you find yourself having trouble distinguishing two (or a few) sounds,
-- you can focus on exactly those. For instance, suppose you've
-- almost completed the above program. You're testing yourself on all
-- the intervals:
-- `earTrainFromChordList $ (\x -> [0,x]) <$> [1..11]`
-- and you keep confusing minor sevenths for major seconds. You can put
-- the giant test on hold to focus on only those two problem intervals:
-- `earTrainFromChordList [[0,2],[0,10]]`
-- Similarly, if you yourself always nailing a certain kind of question,
-- you can remove it from the set of things you're being tested on, to
-- save time.
--
-- Here are the triads, again roughly in order of increasing difficulty:
-- major = [0,4,7]
-- minor = [0,3,7]
-- diminished = [0,3,6]
-- augmented = [0,4,8]
-- You don't have to learn all the intervals before you start learning
-- the triads. I think Burges introduces major and minor chords around
-- the same time as major and minor thirds -- maybe even before.
--
-- Here are the common 7th chords, again in roughly increasing difficulty:
-- dom 7 = [0,4,7,10]
-- min 7 = [0,3,7,10]
-- maj 7 = [0,4,7,11]
-- (fully) diminished 7 = [0,3,6,9]
-- half-diminished 7 = [0,3,6,10]
-- aug 7 = [0,4,8,10]
-- min maj 7 = [0,3,7,11]
-- aug maj 7 = [0,4,8,11]
--
-- One minor complication is that chords can be played in "inverted position",
-- where some notes are displaced by one or more octaves from where they lie
-- in the standard "close position". This software doesn't have a mechanism
-- for handling inversions (yet). If you want, you can work with them by hand.
-- For instance, here are all the inversions of a major triad:
-- [0,4,7] -- Major triad, "root position"
-- [4,7,12] -- Major triad, "first inversion"
-- [7,12,16] -- Major triad, "second inversion"
-- You could also put the chords in "open psition":
-- [0,7,16] -- A wider major triad in root position
-- [4,12,19] -- A wider major triad in first inversion (the inversion is
--              named based only on the note in the root)
-- [7,16,24] -- A wider major triad in second inversion
-- [0,16,31] -- An extremely wide major triad ...
-- The reason I call inversions a "minor" complication is that once you learn
-- to recognize the chords in root position, you've done almost all the work.
-- You'll find yourself recognizing inverted chords even before you learn
-- how to recognize which inversion it is.
--
-- This long comment only describes using `earTrainFromChordList`, because
-- that's the way to get started. Once you have a good vocabulary,
-- the other user-facting functions (the ones that begin with earTrain)
-- will become useful.


-- | = Using Vivid.EarTrain for other stuff: perfect pitch,
--     perfect rhythm, microtonal harmony, distinguishing timbres ...
--
-- If you can pair sounds with labels, `runEarTests` will let you drill
-- yourself on those pairs. It doesn't have to be 12 tone equal temperament;
-- if you replace every instance of 12 with 31 in this file, you would
-- be training for microtonal harmony[1]. You could also train for perfect
-- pitch, or perfect meter (90 bpm! 4 Hz!), or learn to distinguish
-- different kinds of timbres ("pulse wave with a hard low pass filter at
-- 500 Hz!").
--
-- [1] 31 ET is the best microtonal system: https://www.reddit.com/r/haskell/comments/9ivf5e/vivideartrain_learn_to_recognize_intervals_and/e6nk34s/


{-# LANGUAGE ScopedTypeVariables
, DataKinds
, LambdaCase
#-}

module Vivid.EarTrain where

import qualified Data.List as L

import Vivid
import Vivid.Synths
import Vivid.Util


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

pickTestFromPitchSet :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
                   => (a -> [a] -> [a]) -> [a] -> Int -> IO Test
pickTestFromPitchSet howToDelete range numberOfFreqs = do
  freqs <- L.sort <$>
    pickSome' howToDelete numberOfFreqs range -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      showFreqs = putStrLn $ "  bass: " ++ show bass
                               ++ " semitones above A (220 Hz)\n"
                             ++ "  chord: " ++ show normFreqs
                               ++ " relative to the bass"
      playSound = playFreqs $ fmap (et12toFreq 220) freqs :: IO ()
  return (playSound, showFreqs)

pickTestFromScale :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
  => [a] -> Int -> IO Test
pickTestFromScale = pickTestFromPitchSet L.delete

_pickChromaticTest :: (Num a, Eq a, Ord a, Enum a, Show a, Real a, Floating a)
                   => (a -> [a] -> [a]) -> Int -> Int -> IO Test
_pickChromaticTest howToDelete numberOfFreqs range =
  pickTestFromPitchSet howToDelete (map fromIntegral [0..range]) numberOfFreqs

pickChromaticTest :: Int -> Int -> IO Test
pickChromaticTest = _pickChromaticTest L.delete

-- | play two chords in serial
pickChromatic2SerialTest :: Int -> Int -> IO Test
pickChromatic2SerialTest numFreqs range = do
  (q,a) <- pickChromaticTest numFreqs range
  (q',a') <- pickChromaticTest numFreqs range
  return (q >> q', a >> a')

pick3ClusterFreeTest :: Int -> Int -> IO Test
pick3ClusterFreeTest = _pickChromaticTest no3Clusters


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


-- | = Making sounds

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.1 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait (2 :: Int)
  mapM_ free synths

et12toFreq :: Floating a => a -> a -> a
et12toFreq baseFreq p = 2**(p/31) * baseFreq
