module Vivid.Jbb.Dispatch.Act3 where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Lens (over)
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Dispatch.ActNow
import Vivid.Jbb.Dispatch.Config (frameDuration)
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Synths
import Vivid.Jbb.Util


actNew3 :: SynthRegister3 -> Action -> IO (SynthRegister3 -> SynthRegister3)
actNew3 reg (New Boop name) = case M.lookup name $ _boops3 reg of
    Nothing -> do s <- synth boop ()
                  return $ over boops3 $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id
actNew3 reg (New Vap name) = case M.lookup name $ _vaps3 reg of
    Nothing -> do s <- synth vap ()
                  return $ over vaps3 $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id
actNew3 reg (New Sqfm name) = case M.lookup name $ _sqfms3 reg of
    Nothing -> do s <- synth sqfm ()
                  return $ over sqfms3 $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id
actNew3 _ (Send _ _ _) = error $ "actNew3 received a Send."
actNew3 _ (Free _ _)   = error $ "actNew3 received a Free."

actFree3 :: SynthRegister3 -> Time -> Action
         -> IO (SynthRegister3 -> SynthRegister3)
actFree3 reg when (Free Boop name) = case M.lookup name $ _boops3 reg of
  Nothing -> do writeTimeAndError
                  $ "There is no Boop named " ++ name ++ "to free."
                return id
  Just s -> do doScheduledAt (Timestamp when) $ free s
               return $ over boops3 $ M.delete name
actFree3 reg when (Free Vap name) = case M.lookup name $ _vaps3 reg of
  Nothing -> do writeTimeAndError
                  $ "There is no Vap named " ++ name ++ "to free."
                return id
  Just s -> do doScheduledAt (Timestamp when) $ free s
               return $ over vaps3 $ M.delete name
actFree3 reg when (Free Sqfm name) = case M.lookup name $ _sqfms3 reg of
  Nothing -> do writeTimeAndError
                  $ "There is no Sqfm named " ++ name ++ "to free."
                return id
  Just s -> do doScheduledAt (Timestamp when) $ free s
               return $ over sqfms3 $ M.delete name
actFree3 _ _ (Send _ _ _) = error "actFree3 received a Send3."
actFree3 _ _ (New _ _)    = error "actFree3 received a New3."

actSend3 :: SynthRegister3 -> Time -> Action -> IO ()
actSend3 reg when (Send Boop name msg) = case M.lookup name $ _boops3 reg of
  Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
  Just synth -> doScheduledAt (Timestamp when) $ set' synth $ boopMsg msg
actSend3 reg when (Send Vap name msg) = case M.lookup name $ _vaps3 reg of
  Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
  Just synth -> doScheduledAt (Timestamp when) $ set' synth $ vapMsg msg
actSend3 reg when (Send Sqfm name msg) = case M.lookup name $ _sqfms3 reg of
  Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
  Just synth -> doScheduledAt (Timestamp when) $ set' synth $ sqfmMsg msg
actSend3 _ _ (Free _ _) = error "actFree3 received a Send3."
actSend3 _ _ (New _ _)  = error "actFree3 received a New3."

--replaceAll3 :: Dispatch3 -> M.Map MuseqName (Museq Action) -> IO ()
--replaceAll3 dist masNew = do
--  masOld <- takeMVar $ mTimeMuseqs3 dist
--  reg3 <-   takeMVar $ mReg3        dist
--
--  let toFree, toCreate :: [(SynthDefEnum, SynthName)]
--      (toFree,toCreate) = museqsDiff masOld masNew
--
----  mapM_ (act reg3 . uncurry New)                            toCreate
----  mapM_ (act reg3 . (\(sd,name) -> Send sd name ("amp",0))) toFree
----    -- TODO: This should wait until the next Frame
----  wait 0.05 -- wait for the silence just sent to take effect
----  mapM_ (act reg3 . uncurry Free)                           toFree
----
----  putMVar (mTimeMuseqs dist) $ M.map (0,) masNew
----  putMVar (mReg3 dist) $ reg3'
--  
--  return ()

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

  mapM_ (uncurry $ actSend3 reg3) evs

  putMVar (mTime03       dist) time0
  putMVar (mTempoPeriod3 dist) tempoPeriod
  putMVar (mTimeMuseqs3  dist) timeMuseqs
  putMVar (mReg3 dist)         reg3

  wait $ np0 - now
  dispatchLoop3 dist
