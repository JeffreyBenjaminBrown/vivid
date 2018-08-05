module Vivid.Jbb.Distrib.Loop where

import Control.Concurrent.MVar

import Vivid
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Types


-- | Period is the inverse of tempo.
loop :: SynthRegister -> MVar Duration -> MVar Museq -> IO (IO ())
loop reg mPeriod mMuseq = do
  mTime0 <- (\x -> x-0.05) . unTimestamp <$> getTime >>= newMVar
    -- subtract .1 so music starts in .05 seconds, not frameDur seconds

  let loop :: IO ()
      loop = do now <- unTimestamp <$> getTime
                return () -- TODO

  return $ return () -- TODO

  -- it seems safe, and simpler, to use nextEvents_timeUntil and not this
    -- let frameDur = 1 -- music is rendered frameDur seconds at a time
