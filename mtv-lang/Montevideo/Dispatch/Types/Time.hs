module Montevideo.Dispatch.Types.Time (
    Time(..), Duration, RTime(..), RDuration
  , Start, End, RStart, REnd
  , timestamp, unTimestamp ) where

import Vivid (Timestamp(..))


-- | = Time

-- | I think these are meant to be absolute times.
newtype Time   = Time { _unTime :: Rational}
  deriving (Show, Eq, Ord)
type Start     = Time
type End       = Time
type Duration  = Time

-- | TODO ? Maybe `RTime` means "relative time" --
-- relative to something, e.g. the global cycle duration.
-- Or maybe (I hope not) it means "wrapped time"?
newtype RTime  = RTime {_unRTime :: Rational}
  deriving (Show, Ord, Eq)
type RStart    = RTime
type REnd      = RTime
type RDuration = RTime

instance Num Time where
  (+) (Time t) (Time s) = Time (t + s)
  (-) (Time t) (Time s) = Time (t - s)
  (*) (Time t) (Time s) = Time (t * s)
  negate (Time t) = Time (negate t)
  abs (Time t) = Time (abs t)
  signum (Time t) = Time (signum t)
  fromInteger int = Time (fromInteger int)

instance Num RTime where
  (+) (RTime t) (RTime s) = RTime (t + s)
  (-) (RTime t) (RTime s) = RTime (t - s)
  (*) (RTime t) (RTime s) = RTime (t * s)
  negate (RTime t) = RTime (negate t)
  abs (RTime t) = RTime (abs t)
  signum (RTime t) = RTime (signum t)
  fromInteger int = RTime (fromInteger int)

instance Fractional Time where
  (/) (Time t) (Time s) = Time (t / s)
  recip (Time t) = Time (recip t)
  fromRational rat = Time rat

instance Fractional RTime where
  (/) (RTime t) (RTime s) = RTime (t / s)
  recip (RTime t) = RTime (recip t)
  fromRational rat = RTime rat

instance Real Time where
  toRational (Time t) = t

instance Real RTime where
  toRational (RTime t) = t

instance RealFrac Time where
  properFraction (Time r0) = let (i,r) = properFraction r0
                              in (i,Time r)
  truncate (Time r) = truncate r
  round (Time r) = round r
  ceiling (Time r) = ceiling r
  floor (Time r) = floor r

instance RealFrac RTime where
  properFraction (RTime r0) = let (i,r) = properFraction r0
                              in (i,RTime r)
  truncate (RTime r) = truncate r
  round (RTime r) = round r
  ceiling (RTime r) = ceiling r
  floor (RTime r) = floor r

unTimestamp :: Timestamp -> Time
unTimestamp (Timestamp x) = Time $ toRational x

timestamp :: Time -> Timestamp
timestamp = Timestamp . fromRational . _unTime
