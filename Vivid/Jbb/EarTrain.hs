-- | = How to use the ear training function here.
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


{-# LANGUAGE ScopedTypeVariables
, DataKinds
, LambdaCase
#-}

module Vivid.Jbb.EarTrain where

import Data.List as L

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Util (pickSome)


type PlayQuestion = IO () -- ^ make a sound, for the user to identify
type ShowAnswer = IO () -- ^ display something, e.g. "it was a major chord"
type Test = (PlayQuestion, ShowAnswer)


-- | = The top IO function

earTrain :: IO Test -> IO ()
earTrain pickTest = pickTest >>= runTest where
  runTest :: Test -> IO ()
  runTest test@(playFreqs, showFreqs) = do
    showChoices
    playFreqs
    getChar >>= \case
      'a' -> putStrLn "nother sound!\nBTW, the last sound was:"
             >> showFreqs >> earTrain pickTest
      's' -> putStrLn "how what was played" >> showFreqs
             >> runTest test
      'q' -> putStrLn "uit" >> return ()
      otherwise -> putStrLn "replay" >> runTest test

showChoices :: IO ()
showChoices = putStrLn $ "\nPlease press a key:\n"
                      ++ "  s: (s)how what was played\n"
                      ++ "  a: play (a)nother chord\n"
                      ++ "  q: (q)uit,\n"
                      ++ "  any other key: replay the sound."


-- | = pick uniformly from a chromatic range

earTrainChromatic :: Int -> Int -> IO ()
earTrainChromatic numberOfFreqs range =
  earTrain $ pickChromaticTest numberOfFreqs range

pickChromaticTest :: Int -> Int -> IO Test
pickChromaticTest numberOfFreqs range = do
  freqs <- L.sort <$>
    pickSome numberOfFreqs [0 .. fromIntegral $ range-1] -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      showFreqs = putStrLn $ "  bass: " ++ show bass
                               ++ " semitones above A (220 Hz)\n"
                             ++ "  chord: " ++ show normFreqs
                               ++ " relative to the bass"
      playSound = playFreqs $ fmap (et12toFreq 220) freqs :: IO ()
  return (playSound, showFreqs)


-- | = pick from a custom list of chords

earTrainFromChordList :: [[Float]] -> IO ()
earTrainFromChordList chords = earTrain $ pickTestFromChordList chords

pickTestFromChordList :: [[Float]] -> IO Test
pickTestFromChordList chords = do
  transpose <- pick [0..11]
  freqs <- pick chords
  let playSound = playFreqs $ fmap (et12toFreq 220)
                  $ fmap ((+) transpose) freqs :: IO ()
      showFreqs = putStrLn $ "  transpose: " ++ show transpose
                           ++ "\n  chord: " ++ show freqs
  return (playSound, showFreqs)


-- | = Making sounds

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.1 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait 1
  mapM_ free synths

et12toFreq :: Floating a => a -> a -> a
et12toFreq baseFreq p = 2**(p/12) * baseFreq
