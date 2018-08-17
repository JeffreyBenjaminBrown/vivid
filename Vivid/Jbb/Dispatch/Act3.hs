module Vivid.Jbb.Dispatch.Act3 where

import Control.Concurrent (forkIO, ThreadId)
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


startDispatchLoop3 :: Dispatch3 -> IO ThreadId
startDispatchLoop3 dist = do
  tryTakeMVar $ mTime03 dist -- empty it, just in case
  (+(frameDuration * (-0.8))) . unTimestamp <$> getTime
    -- subtract nearly an entire frameDuration so it starts sooner
    >>= putMVar (mTime03 dist)
  forkIO $ dispatchLoop3 dist

dispatchLoop3 :: Dispatch3 -> IO ()
dispatchLoop3 dist = do
  time0  <-      takeMVar $ mTime03       dist
  tempoPeriod <- takeMVar $ mTempoPeriod3 dist
  timeMuseqs <-  takeMVar $ mTimeMuseqs3  dist
  reg3 <-        takeMVar $ mReg3         dist
  now <- unTimestamp <$> getTime

  let museqs = M.elems timeMuseqs :: [Museq Action]
      np0 = nextPhase0 time0 frameDuration now
      nextRender = np0 + frameDuration
      evs = concatMap f museqs :: [(Time,Action)] where
        f = arc time0 tempoPeriod nextRender $ nextRender + frameDuration

  mapM_ (scheduleSend reg3) evs

  putMVar (mTime03       dist) time0
  putMVar (mTempoPeriod3 dist) tempoPeriod
  putMVar (mTimeMuseqs3  dist) timeMuseqs
  putMVar (mReg3 dist)         reg3

  wait $ np0 - now
  dispatchLoop3 dist
