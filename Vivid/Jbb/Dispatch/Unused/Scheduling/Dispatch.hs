{-# LANGUAGE TupleSections #-}

module Vivid.Jbb.Dispatch.Dispatch (
  -- | user-facing
  replaceAll
  , replace
  , chTempoPeriod
  , startDispatchLoop

  -- | = backend
  , dispatchLoop
  , allWaiting
  , showDist
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Random
import System.Random.MWC

import Vivid
import Vivid.Jbb.Dispatch.ActNow
import Vivid.Jbb.Dispatch.Config
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


-- | = user-facing functions

replaceAll :: Dispatch -> M.Map MuseqName (Museq Action) -> IO ()
replaceAll dist masNew = do
  masOld <- M.map snd <$> readMVar (mTimeMuseqs dist)
    :: IO (M.Map MuseqName (Museq Action))
  let (toFree,toCreate) = museqsDiff masOld masNew
  mapM_ (act (reg dist) . uncurry New)                            toCreate
  mapM_ (act (reg dist) . (\(sd,name) -> Send sd name ("amp",0))) toFree
  wait 0.05 -- wait for the silence just sent to take effect
  mapM_ (act (reg dist) . uncurry Free)                           toFree
  swapMVar (mTimeMuseqs dist) $ M.map (0,) masNew
  return ()

replace :: Dispatch -> MuseqName -> Museq Action -> IO ()
replace dist newName newMuseq = do
  masOld <- M.map snd <$> readMVar (mTimeMuseqs dist)
    :: IO (M.Map MuseqName (Museq Action))
  let masNew = M.insert newName newMuseq masOld
  replaceAll dist masNew

-- | If can't change period now (because some Museq is not waiting),
-- wait between 5 and 10 ms, then retry
chTempoPeriod :: Dispatch -> Duration -> IO ()
chTempoPeriod dist newPeriod = try where
  try = do
    waitingUntils <- map fst . M.elems <$> readMVar (mTimeMuseqs dist)
    now <- unTimestamp <$> getTime
    if and $ map (> now) waitingUntils
      then swapMVar (mTempoPeriod dist) newPeriod >> return ()
      else do mwc <- createSystemRandom
              wait =<< (sampleFrom mwc
                        $ (*(1/1000)) . (+5) . (*5) <$> stdUniform
                        :: IO Double)
              try

startDispatchLoop :: Dispatch -> IO ThreadId
startDispatchLoop dist = do
  tryTakeMVar $ mTime0 dist -- empty it, just in case
  (+(-0.05)) . unTimestamp <$> getTime >>= putMVar (mTime0 dist)
    -- add .05 so music starts in .05 seconds, not frameDur seconds
  forkIO $ dispatchLoop dist


-- | = backend

dispatchLoop :: Dispatch -> IO ()
dispatchLoop dist = do
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
  tempoPeriod <- readMVar $ mTempoPeriod dist
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  now <- unTimestamp <$> getTime -- get time ALAP

  -- -- for debugging
  -- putStrLn $ "\n" ++ show (now - time0)
  -- putStrLn =<< showDist dist

  -- find what comes next in each Museq
  let nextPlus :: M.Map MuseqName (Duration, [Action])
        -- some of these are immediately next, but maybe not all
      nextPlus = M.map (findNextEvents time0 tempoPeriod now . snd) timeMuseqs

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

  dispatchLoop dist

allWaiting :: Dispatch -> IO (Bool)
allWaiting dist = do
  timeMuseqs <- readMVar $ mTimeMuseqs dist
  let times = map fst $ M.elems $ timeMuseqs
  now <- unTimestamp <$> getTime
  return $ and $ map (> now) times

-- | todo : this blocks if any MVar is empty
showDist :: Dispatch -> IO String
showDist dist = do timeMuseqs <- readMVar $ mTimeMuseqs dist
                   reg' <- showSynthRegister $ reg dist
                   time0 <- readMVar $ mTime0 dist
                   tempoPeriod <- readMVar $ mTempoPeriod dist
                   return $ "(Time,Museq)s: " ++ show timeMuseqs
                     ++ "\nSynthRegister: " ++ show reg'
                     ++ "\nTime 0: " ++ show time0
                     ++ "\nPeriod: " ++ show tempoPeriod
