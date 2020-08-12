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
  spacings = filter (feasibleSpacing modulus) [5 .. 40]
  r1 = [ (s, thanos' modulus s edo)
       | s <- spacings ]
  r2 = filter ((<= maxStretch) . fst . snd) r1
  in mapM_ (\(spacing, (maxFretDiff, formatted)) -> do
              putStrLn $ "\n" ++ show spacing
                ++ ", max reach: " ++ show maxFretDiff
                ++ ", or in 12-edo, "
                ++ show ( fromIntegral (12 * maxFretDiff * modulus)
                         / fromIntegral edo)
              myPrint $ formatted )
     r2

thanos' modulus spacing edo = let
  notes = importantNotes M.! edo
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
  let spaces = zip [0..] $ fmap (*spacing) [0..8]
      a = fmap (_2 %~ (edoStep -)) spaces
      b = filter ((== 0) . flip mod modulus . snd) a
      (string,fret) = minimumBy (comparing (abs . snd)) b
  in (string, div fret modulus)

feasibleSpacing :: Int -> Int -> Bool
feasibleSpacing modulus spacing = 
  elem 1 $ fmap (flip mod modulus . (*) spacing) [1..modulus]

importantNotes = M.fromList
  [ (87, [ (28, 5  % 4)
        , (40, 11 % 8)
        , (51, 3  % 2)
        , (61, 13 % 8)
        , (70, 7  % 4)
        , (87, 2  % 1) ] )
  , (58, [ (19, 5 / 4 )
         , (27, 11 / 8)
         , (34, 3 / 2)
         , (41, 13 / 8)
         , (47, 7 / 4)
         , (58, 2 ) ] )
  , (46, [ (15, 5 % 4)
         , (27, 3 % 2)
         , (21, 11 % 8)
         , (32, 13 % 8)
         , (37, 7 % 4)
        ,  (46, 2) ] )
  , (41, [ (13, 5 / 4)
         , (19, 11 / 8)
         , (24, 3/2)
         , (29, 13/8)
         , (33, 7/4)
         , (41, 2/2) ] )
  ]
