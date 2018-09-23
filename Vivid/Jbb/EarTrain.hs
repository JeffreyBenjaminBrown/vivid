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
-- (5) Run "earTrain n r", where:
--     n is the number of notes you want to be quizzed on at a time.
--     r is the size (measured in halfsteps) of the range of pitches
--       you would like to be quizzed on. (You'll probably want to
--       use a multiple of 12.)
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


earTrain :: Int -> Int -> IO ()
  -- 3 kinds of IO: randomness, sound and text
earTrain numberOfFreqs range = do
  freqs <- L.sort <$>
    pickSome numberOfFreqs [0 .. fromIntegral $ range-1] -- randomness
  let bass = minimum freqs
      normFreqs = fmap (\n -> n - bass) freqs
      showFreqs = do putStrLn $ "  bass: " ++ show bass
                                  ++ " semitones above A (220 Hz)\n"
                                ++ "  chord: " ++ show normFreqs
                                  ++ " relative to the bass"
      theSound = playFreqs $ fmap (et12toFreq 220) freqs :: IO ()
      soundAndText :: IO () -> IO ()
      soundAndText sound = do -- 2 kinds of IO: sound(out) and text(in & out)
        putStrLn $ "\nPlease press a key:\n"
          ++ "  s: (s)how what was played\n"
          ++ "  a: play (a)nother chord\n"
          ++ "  q: (q)uit,\n"
          ++ "  any other key: replay the sound."
        sound
        getChar >>= \case
          'a' -> putStrLn "nother sound!\nBTW, the last sound was:"
                 >> showFreqs >> earTrain numberOfFreqs range
          's' -> putStrLn "how what was played" >> showFreqs
                 >> soundAndText sound
          'q' -> putStrLn "uit" >> return ()
          otherwise -> putStrLn "replay" >> soundAndText sound
  soundAndText theSound

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.2 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait 1
  mapM_ free synths

et12toFreq :: Floating a => a -> a -> a
et12toFreq baseFreq p = 2**(p/12) * baseFreq
