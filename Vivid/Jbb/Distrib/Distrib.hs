module Vivid.Jbb.Distrib.Distrib where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import qualified Data.Map as M

import Vivid
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Museq
import Vivid.Jbb.Distrib.Types


allWaiting :: Distrib -> IO (Bool)
allWaiting dist = do
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  let times = map fst $ M.elems $ timeMuseqs
  now <- unTimestamp <$> getTime
  return $ and $ map (> now) times

--chPeriod :: Distrib -> IO ()
--chPeriod = do
--  waitingUntil <- readMVar mWaitingUntil
--  now <- unTimestamp <$> getTime

startDistribLoop :: Distrib -> IO ThreadId
startDistribLoop dist = do
  tryTakeMVar $ mTime0 dist -- empty it, just in case
  (+(-0.05)) . unTimestamp <$> getTime >>= putMVar (mTime0 dist)
    -- subtract .1 so music starts in .05 seconds, not frameDur seconds
  forkIO $ distribLoop dist

distribLoop :: Distrib -> IO ()
distribLoop dist = do
  time0  <- readMVar $ mTime0  dist
  period <- readMVar $ mPeriod dist
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  now <- unTimestamp <$> getTime -- get time ALAP

  -- find what comes next in each Museq
  let nextPlus :: M.Map String (Duration, [Action])
        -- some of these are immediately next, but maybe not all
      nextPlus = M.map (findNextEvents time0 period now . snd) timeMuseqs

  -- record (ASAP) in each Museq the time until its next Action(s)
  swapMVar (mTimeMuseqs dist) $ M.mapWithKey
    (\name (_,vec) -> (fst $ (M.!) nextPlus name, vec))
    timeMuseqs

  let nextTime = minimum $ M.elems $ M.map fst nextPlus
      nextActions = concatMap snd
                    $ filter ((== nextTime) . fst)
                    $ M.elems nextPlus

  wait $ nextTime - now
  mapM_ (act $ reg dist) nextActions
  distribLoop dist
