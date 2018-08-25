{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Dispatch.Dispatch where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens (over, _1)
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


-- | = Act on a Msg

-- todo ? `act` might never get used
mapAct :: SynthRegister -> Time -> MapAction
     -> IO (SynthRegister -> SynthRegister)
mapAct reg t a@(MapSend _ _ _) = mapActSend reg t a >> return id
mapAct reg t a@(MapFree _ _)   = mapActFree reg t a
mapAct reg t a@(MapNew _ _)    = mapActNew  reg   a

mapActNew :: SynthRegister -> MapAction -> IO (SynthRegister -> SynthRegister)
mapActNew reg (MapNew Boop name) = case M.lookup name $ _boops reg of
    Nothing -> do s <- synth boop ()
                  return $ over boops $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id
mapActNew reg (MapNew Vap name) = case M.lookup name $ _vaps reg of
    Nothing -> do s <- synth vap ()
                  return $ over vaps $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id
mapActNew reg (MapNew Sqfm name) = case M.lookup name $ _sqfms reg of
    Nothing -> do s <- synth sqfm ()
                  return $ over sqfms $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id
mapActNew _ (MapSend _ _ _) = error $ "mapActNew received a MapSend."
mapActNew _ (MapFree _ _)   = error $ "mapActNew received a MapFree."

mapActFree reg when (MapFree Boop name) = case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over boops $ M.delete name
mapActFree reg when (MapFree Vap name) = case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over vaps $ M.delete name
mapActFree reg when (MapFree Sqfm name) = case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over sqfms $ M.delete name
mapActFree _ _ (MapSend _ _ _) = error "mapActFree received a MapSend."
mapActFree _ _ (MapNew _ _)    = error "mapActFree received a MapNew."

mapActSend :: SynthRegister -> Time -> MapAction -> IO ()
mapActSend reg when (MapSend Boop name msgs) =
  case M.lookup name $ _boops reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ boopMapMsg msgs
mapActSend reg when (MapSend Sqfm name msgs) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ sqfmMapMsg msgs
mapActSend reg when (MapSend Vap name msgs) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ vapMapMsg msgs
mapActSend _ _ (MapFree _ _) = error "mapActFree received a MapSend."
mapActSend _ _ (MapNew _ _)  = error "mapActFree received a MapNew."


-- | = Change the music

mapReplace :: MapDispatch -> MuseqName -> Museq MapAction -> IO ()
mapReplace disp newName newMuseq = do
  masOld <- readMVar $ mapMMuseqs disp
  mapReplaceAll disp $ M.insert newName newMuseq masOld

-- | ASSUMES every synth has an "amp" parameter which, when 0, causes silence.
--
-- Strategy: Upon changing Museqs, compute which synths need deletion,
-- and which need creation. Create the latter immediately. Wait to delete
-- the former until it's safe, and before deleting them, send silence
-- (set "amp" to 0).
mapReplaceAll :: MapDispatch -> M.Map MuseqName (Museq MapAction) -> IO ()
mapReplaceAll disp masNew = do
  time0  <-      takeMVar $ mapMTime0       disp
  tempoPeriod <- takeMVar $ mapMTempoPeriod disp
  masOld <-      takeMVar $ mapMMuseqs      disp
  reg <-         takeMVar $ mapMReg         disp
  now <- unTimestamp <$> getTime

  let when = nextPhase0 time0 frameDuration now + 2 * frameDuration
        -- `when` = the start of the first not-yet-rendered frame
      toFree, toCreate :: [(SynthDefEnum, SynthName)]
      (toFree,toCreate) = mapMuseqsDiff masOld masNew

  newTransform  <- mapM (mapActNew  reg)      $ map (uncurry MapNew)  toCreate
  freeTransform <- mapM (mapActFree reg when) $ map (uncurry MapFree) toFree

  putMVar (mapMTime0       disp) time0       -- unchnaged
  putMVar (mapMTempoPeriod disp) tempoPeriod -- unchanged
  putMVar (mapMMuseqs      disp) masNew
  putMVar (mapMReg         disp) $ foldl (.) id newTransform reg

  forkIO $ do wait $ when - now -- delete register's synths once it's safe
              reg <-takeMVar $ mapMReg disp
              putMVar (mapMReg disp) $ foldl (.) id freeTransform reg

  return ()

mapChTempoPeriod :: MapDispatch -> Duration -> IO ()
mapChTempoPeriod disp newTempoPeriod = do
  time0       <- takeMVar $ mapMTime0       disp
  tempoPeriod <- takeMVar $ mapMTempoPeriod disp
  now         <- unTimestamp <$> getTime
  let np0 = nextPhase0 time0 frameDuration now
      startRender = np0 + 2 * frameDuration
        -- np0 and startRender are defined similar to dispatchLoop,
        -- EXCEPT: multiply frameDuration by 2, because np0 is one period less
        -- than it will be the next time dispatchLoop runs
      startRenderInCycles = (startRender - time0) / tempoPeriod
      newTime0 = startRender - startRenderInCycles * newTempoPeriod
  putMVar (mapMTempoPeriod disp) newTempoPeriod
  putMVar (mapMTime0       disp) newTime0


-- | = The Dispatch loop

mapStartDispatchLoop :: MapDispatch -> IO ThreadId
mapStartDispatchLoop disp = do
  tryTakeMVar $ mapMTime0 disp -- empty it, just in case
  mbTempo <- tryReadMVar $ mapMTempoPeriod disp
  maybe (putMVar (mapMTempoPeriod disp) 1) (const $ return ()) mbTempo
  
  (+(frameDuration * (-0.8))) . unTimestamp <$> getTime
    -- subtract nearly an entire frameDuration so it starts sooner
    >>= putMVar (mapMTime0 disp)
  forkIO $ mapDispatchLoop disp

mapDispatchLoop :: MapDispatch -> IO ()
mapDispatchLoop disp = do
  time0  <-      takeMVar $ mapMTime0       disp
  tempoPeriod <- takeMVar $ mapMTempoPeriod disp
  museqsMap <-   takeMVar $ mapMMuseqs      disp
  reg <-         takeMVar $ mapMReg         disp
  now <- unTimestamp <$> getTime

  let np0 = nextPhase0 time0 frameDuration now
      startRender = np0 + frameDuration
      evs = concatMap f $ M.elems museqsMap :: [(Time, MapAction)] where
        f :: Museq MapAction -> [(Time, MapAction)]
        f m = map (over _1 fst)  -- use `fst` to ignore message's end time
          $ arc time0 tempoPeriod startRender (startRender + frameDuration) m

  mapM_ (uncurry $ mapActSend reg) evs

  putMVar (mapMTime0       disp) time0
  putMVar (mapMTempoPeriod disp) tempoPeriod
  putMVar (mapMMuseqs      disp) museqsMap
  putMVar (mapMReg         disp) reg

  wait $ fromRational np0 - now
  mapDispatchLoop disp

showEvs evs = concatMap (\(t,a) -> "\n" ++ show t ++ ": " ++ show a) evs
