-- This is hopefully better than Thanos.

module Montevideo.JI.Thanos.Thanos2 where

import Prelude hiding (span)

import Control.Lens
import           Data.List hiding (span)
import           Data.Ord
import           Data.Ratio

import Montevideo.Util hiding (tr)
import Montevideo.JI.Thanos.Thanos
import Montevideo.JI.Lib


-- | Gives the (string, fret) position of an interval.
-- PITFALL: Fails (head of []) if the gaps are not relatively prime.
guitarSpot :: Integral a => a -> a -> a -> (a, a)
guitarSpot stringGap fretGap interval =
  head [ (string, fret) |
         fret <- [0..],
         let evenMultiple = interval - fret*fretGap
             string = div evenMultiple stringGap,
         mod evenMultiple stringGap == 0 ]
