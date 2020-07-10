module Montevideo.Dispatch.Util (
    nextPhase0
  , prevPhase0
  ) where


-- | `time0` is the first time that had phase 0
-- | TODO ? rewrite using div', mod' from Data.Fixed
nextPhase0 :: RealFrac a => a -> a -> a -> a
nextPhase0 time0 period now =
  fromIntegral x * period + time0
  where x :: Int = ceiling $ (now - time0) / period

-- | PITFALL ? if lastPhase0 or nextPhase0 was called precisely at phase0,
-- both would yield the same result. Since time is measured in microseconds
-- there is exactly a one in a million chance of that.
-- | TODO ? rewrite using div', mod' from Data.Fixed
prevPhase0 :: RealFrac a => a -> a -> a -> a
prevPhase0 time0 period now =
  fromIntegral x * period + time0
  where x :: Int = floor $ (now - time0) / period
