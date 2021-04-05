-- | This compares grid arrangements of EDOs,
-- by showing how far your hand must stray from the
-- column containing note 0 in order to reach
-- (the approximations to) any of the first few harmonics.
-- For instance, to reveal that the only arrangement of 87-edo
-- with a maximum deviation of less than 7 is
-- the one that puts 10\87 between each column:
-- > myPrint $ compareGrids_filt 6 87
-- (10,4,9,[1,-2,0,0,1])
--
-- TODO : Make this consider the placement of the octave,
-- just like it considers the placement of 3/2.
-- (That probably means modifying other modules.)

{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.JI.Grid where

import Control.Lens

import Montevideo.Util
import Montevideo.JI.Lib


compareGrids_filt :: Int -> Int -> [(Int,Int,Int,Int,[Int])]
compareGrids_filt maxSummedDev =
  filter ((<= maxSummedDev) . (^. _4))
  . compareGrids

compareGrids :: Int -> [(Int,Int,Int,Int,[Int])]
compareGrids edo0 = let
  width n = round
            (fromIntegral edo0 / fromIntegral n :: Double)
  als n = alignments n edo0
  in [ ( n            -- distance (as a fraction of edo0) between columns
       , width n      -- which column the probably-nearest octave lands in
       , myMod edo0 n -- which row    the probably-nearest octave lands in
       , sum $ map abs $ als n -- sum of errors
       , als n) -- errors
     | n <- [6..20]]

alignments :: Int -> Int -> [Int]
alignments spacing edo0 =
  map ( flip myMod spacing
        . fromIntegral
        . (^. _1)
        . best (fromIntegral edo0) )
  [3/2,5/4,7/4,11/8,13/8]
