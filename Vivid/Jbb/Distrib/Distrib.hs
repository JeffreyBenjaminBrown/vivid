module Vivid.Jbb.Distrib.Distrib where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Random
import System.Random.MWC


import Vivid
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Museq
import Vivid.Jbb.Distrib.Types


-- | todo : this blocks if any MVar is empty
showDist :: Distrib -> IO String
showDist dist = do timeMuseqs <- readMVar $ mTimeMuseqs dist
                   reg' <- showSynthRegister $ reg dist
                   time0 <- readMVar $ mTime0 dist
                   period <- readMVar $ mPeriod dist
                   return $ "(Time,Museq)s: " ++ show timeMuseqs
                     ++ "\nSynthRegister: " ++ show reg'
                     ++ "\nTime 0: " ++ show time0
                     ++ "\nPeriod: " ++ show period

allWaiting :: Distrib -> IO (Bool)
allWaiting dist = do
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  let times = map fst $ M.elems $ timeMuseqs
  now <- unTimestamp <$> getTime
  return $ and $ map (> now) times

-- | If can't change period now (because some Museq is not waiting),
-- wait between 5 and 10 ms, then retry
chPeriod :: Distrib -> Duration -> IO ()
chPeriod dist newPeriod = try where
  try = do
    waitingUntils <- map fst . M.elems <$> readMVar (mTimeMuseqs dist)
    now <- unTimestamp <$> getTime
    if and $ map (> now) waitingUntils
      then swapMVar (mPeriod dist) newPeriod >> return ()
      else do mwc <- createSystemRandom
              wait =<< (sampleFrom mwc
                        $ (*(1/1000)) . (+5) . (*5) <$> stdUniform
                        :: IO Double)
              try

startDistribLoop :: Distrib -> IO ThreadId
startDistribLoop dist = do
  tryTakeMVar $ mTime0 dist -- empty it, just in case
  (+(-0.05)) . unTimestamp <$> getTime >>= putMVar (mTime0 dist)
    -- subtract .1 so music starts in .05 seconds, not frameDur seconds
  forkIO $ distribLoop dist

distribLoop :: Distrib -> IO ()
distribLoop dist = do
  let epsilon = 0.005 :: Duration -- ^ PITFALL:
    -- Events scheduled with a difference
    -- of less than epsilon seconds might play at the same time.
    -- (In learning/schedule.hs.loop, I determined that threadDelay
    -- always lags less than 5 ms.)
    -- Why I think I need that: Suppose at now=0, e and f are scheduled for
    -- time 1 and 1.00001. If I wait 1s and then play e, it could be that
    -- the next scan for upcoming events would take place after f
    -- was scheduled for, in which case f would never happen.
    -- If I play e and f together, and GHC happens to play f again
    -- thereafter, it should not matter, because it's just telling
    -- a synth to keep doing what it's doing.

  time0  <- readMVar $ mTime0  dist
  period <- readMVar $ mPeriod dist
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  now <- unTimestamp <$> getTime -- get time ALAP

  -- -- for debugging
  -- putStrLn $ "\n" ++ show (now - time0)
  -- putStrLn =<< showDist dist

  -- find what comes next in each Museq
  let nextPlus :: M.Map MuseqName (Duration, [Action])
        -- some of these are immediately next, but maybe not all
      nextPlus = M.map (findNextEvents time0 period now . snd) timeMuseqs

  -- record (ASAP) in each Museq the time until its next Action(s)
  swapMVar (mTimeMuseqs dist) $ M.mapWithKey
    (\name (_,vec) -> (now + fst ((M.!) nextPlus name)
                      , vec))
    timeMuseqs

  -- TODO : if the sequence is empty, this errs
  let leastWait = minimum $ M.elems $ M.map fst nextPlus
      nextActions = concatMap snd
                    $ filter ((< leastWait + epsilon) . fst)
                    $ M.elems nextPlus

  wait leastWait
  mapM_ (act $ reg dist) nextActions

  distribLoop dist
