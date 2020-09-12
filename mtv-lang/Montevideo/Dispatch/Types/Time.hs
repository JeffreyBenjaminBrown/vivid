module Montevideo.Dispatch.Types.Time (
    Time, Duration, RTime(..), RDuration
  , Start, End, RStart, REnd
  , unTimestamp ) where

import Vivid (Timestamp(..))


-- | = Time

type Time      = Rational -- ^ a moment in time
type Start     = Time
type End       = Time
type Duration  = Rational -- ^ a length of time

-- | TODO ? Maybe `RTime` means "relative time" --
-- relative to something, e.g. the global cycle duration.
-- Or maybe (I hope not) it means "wrapped time"?
newtype RTime  = RTime {_unRTime :: Rational} deriving (Show,Ord,Eq)
type RStart    = RTime
type REnd      = RTime
type RDuration = RTime

instance Num RTime where
  (+) (RTime t) (RTime s) = RTime (t + s)
  (-) (RTime t) (RTime s) = RTime (t - s)
  (*) (RTime t) (RTime s) = RTime (t * s)
  negate (RTime t) = RTime (negate t)
  abs (RTime t) = RTime (abs t)
  signum (RTime t) = RTime (signum t)
  fromInteger int = RTime (fromInteger int)

instance Fractional RTime where
  (/) (RTime t) (RTime s) = RTime (t / s)
  recip (RTime t) = RTime (recip t)
  fromRational rat = RTime rat

instance Real RTime where
  toRational (RTime t) = t

instance RealFrac RTime where
  properFraction (RTime r0) = let (i,r) = properFraction r0
                              in (i,RTime r)
  truncate (RTime r) = truncate r
  round (RTime r) = round r
  ceiling (RTime r) = ceiling r
  floor (RTime r) = floor r

unTimestamp :: Timestamp -> Time
unTimestamp (Timestamp x) = toRational x
