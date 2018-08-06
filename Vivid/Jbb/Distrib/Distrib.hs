module Vivid.Jbb.Distrib.Distrib where

import Control.Concurrent.MVar
import qualified Data.Map as M

import Vivid
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Types


allWaiting :: Distrib -> IO (Bool)
allWaiting dist = do
  museqs <- readMVar $ mMuseqs dist
  let times = map fst $ M.elems $ museqs
  now <- unTimestamp <$> getTime
  return $ and $ map (> now) times


---- | Period is the inverse of tempo.
--loop :: SynthRegister -> MVar Duration -> MVar Museq -> IO (IO ())
--loop reg mPeriod mMuseq = do
--  mTime0 <- (\x -> x-0.05) . unTimestamp <$> getTime >>= newMVar
--    -- subtract .1 so music starts in .05 seconds, not frameDur seconds
--  mWaitingUntil <- newEmptyMVar
--
--  let go :: IO ()
--      go = do
--        time0  <- readMVar mTime0
--        period <- readMVar mPeriod
--        museq  <- readMVar mMuseq
--        now <- unTimestamp <$> getTime
--        let (timeToWait, nextEvents) =
--              findNextEvents time0 period now museq
--        swapMVar mWaitingUntil $ now + timeToWait
--
--        -- | period (tempo) can only be changed while waiting
--        swapMVar mWaiting True
--        wait timeToWait
--        swapMVar mWaiting False
--
--        map (act reg) nextEvents
--        go
--
--      chPeriod :: IO ()
--      chPeriod = do
--        waitingUntil <- readMVar mWaitingUntil
--        now <- unTimestamp <$> getTime
--
--  loopThread <- forkIO go
--
---- it seems safe, and simpler, to use nextEvents_timeUntil and not this
--  -- let frameDur = 1 -- music is rendered frameDur seconds at a time
