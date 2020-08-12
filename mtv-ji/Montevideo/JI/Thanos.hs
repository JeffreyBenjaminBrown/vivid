-- | Thanos tunings are high-EDO tunings for stringed instruments,
-- in which one skips some frets.
-- This code lets you see how convenient a given tuning is.

module Montevideo.JI.Thanos where

import Control.Lens
import           Data.List
import qualified Data.Map as M
import           Data.Ord
import           Data.Ratio

import Montevideo.Util
import Montevideo.JI.Lib


-- | `thanos''` explores a lot of ways one might
-- tune a guitar to achieve a certain `edo`.
--
-- For instance, to check how including only every third fret
-- works to play in 58 edo, run
-- `thanos'' 3 58 <whatever>`.
thanos'' :: Int -- ^ Make this `n` to keep every `n`th fret.
         -> Int -- ^ The EDO to examine.
         -> Int -- ^ The maximum number of frets you're willing to stretch.
         -> IO ()
thanos'' modulus edo maxStretch = let
  spacings = filter (feasibleSpacing modulus) [5 .. 50]
  r1 = [ (s, thanos' modulus s $ fromIntegral edo)
       | s <- spacings ]
  r2 = filter ((<= maxStretch) . fst . snd) r1
  in mapM_ (\(spacing, (maxFretDiff, formatted)) -> do
              putStrLn $ "\nmod " ++ show modulus
                ++ ", spaced " ++ show spacing ++ "\\" ++ show edo
                ++ ", max reach: " ++ show maxFretDiff
                ++ ", or in 12-edo, "
                ++ show ( fromIntegral (12 * maxFretDiff * modulus)
                         / fromIntegral edo)
              myPrint $ formatted )
     r2

thanos' modulus spacing edo = let
  notes :: [(Int, Rational)] =
    importantNotes edo & traversed . _1 %~ fromIntegral
  maxFretDiff = maximum results' - minimum results'
    where results' = 0 : map snd results
  formatted = zip notes results
  results :: [(Int,Int)] =
    map (thanos modulus spacing . fst) notes
  in (maxFretDiff, formatted)

thanos :: Int -- ^ For the Kite tuning, this would be 2
       -> Int -- ^ For the Kite tuning, this would be 13
       -- (as in 13 steps of 41-edo)
       -> Int -- ^ A step of the Edo one would like to approximate.
       -- For instance, since Kite wanted to be able to reach 24\41 easily,
       -- his list surely included the number 24.
       -> ( Int -- ^ which string to play it on
          , Int ) -- ^ which fret to play it on
thanos modulus spacing edoStep =
  let spaces = zip [0..] $ fmap (*spacing) [0..20]
      a = fmap (_2 %~ (edoStep -)) spaces
      b = filter ((== 0) . flip mod modulus . snd) a
      (string,fret) = minimumBy (comparing (abs . snd)) b
  in (string, div fret modulus)

-- | A modulus-spacing pair is feasible iff they are relatively prime.
-- For instance, in the Kite tuning (which is of course feasible),
-- the modulus is 2, and the spacing is 13.
feasibleSpacing :: Int -> Int -> Bool
feasibleSpacing modulus spacing =
  elem 1 $ fmap (flip mod modulus . (*) spacing) [1..modulus]

importantNotes :: Int        -- ^ An EDO system.
               -> [ ( Int    -- ^ A value from that EDO system.
                    , Rational)] -- ^ The ratio the EDO value represents.
importantNotes edo = let
  primes = [5/4, 11/8, 3%2, 13/8, 7/4, 2]
  edoValues = map (fromIntegral . (^. _1) . best (fromIntegral edo)) primes
  in zip edoValues primes
