module Vivid.Jbb.Dispatch.Act3 where

import Control.Concurrent.MVar
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Dispatch.Config (frameDuration)
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Synths
import Vivid.Jbb.Util


dispatchLoop3 :: Dispatch -> IO ()
dispatchLoop3 dist = do
  time0  <-      takeMVar $ mTime0     dist
  tempoPeriod <- takeMVar $ mPeriod    dist
  timeMuseqs <-  takeMVar $ mTimeMuseqs dist
  now <- unTimestamp <$> getTime

  let museqs = map snd $ M.elems timeMuseqs :: [Museq Action]
      np0 = nextPhase0 time0 frameDuration now
      evs = map f museqs where
        f = arc time0 tempoPeriod np0 $ np0 + frameDuration

  -- >>> TODO NEXT : render that stuff, using doSchedule

  putMVar (mTime0      dist) time0
  putMVar (mPeriod     dist) tempoPeriod
  putMVar (mTimeMuseqs dist) timeMuseqs
  return ()
