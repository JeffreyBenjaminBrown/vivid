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
mapAct :: SynthRegister -> Time -> Action
     -> IO (SynthRegister -> SynthRegister)
mapAct reg t a@(Send _ _ _) = mapActSend reg t a >> return id
mapAct reg t a@(Free _ _)   = mapActFree reg t a
mapAct reg t a@(New _ _)    = mapActNew  reg   a

mapActNew :: SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
mapActNew reg (New Boop name) = case M.lookup name $ _boops reg of
    Nothing -> do s <- synth boop ()
                  return $ over boops $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id
mapActNew reg (New Vap name) = case M.lookup name $ _vaps reg of
    Nothing -> do s <- synth vap ()
                  return $ over vaps $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id
mapActNew reg (New Sqfm name) = case M.lookup name $ _sqfms reg of
    Nothing -> do s <- synth sqfm ()
                  return $ over sqfms $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id
mapActNew _ (Send _ _ _) = error $ "mapActNew received a Send."
mapActNew _ (Free _ _)   = error $ "mapActNew received a Free."

mapActFree reg when (Free Boop name) = case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over boops $ M.delete name
mapActFree reg when (Free Vap name) = case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over vaps $ M.delete name
mapActFree reg when (Free Sqfm name) = case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> let fr = fromRational in
    do doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
       doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
       return $ over sqfms $ M.delete name
mapActFree _ _ (Send _ _ _) = error "mapActFree received a Send."
mapActFree _ _ (New _ _)    = error "mapActFree received a New."

mapActSend :: SynthRegister -> Time -> Action -> IO ()
mapActSend reg when (Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ boopMsg msg
mapActSend reg when (Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ sqfmMsg msg
mapActSend reg when (Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' synth) $ vapMsg msg
mapActSend _ _ (Free _ _) = error "mapActFree received a Send."
mapActSend _ _ (New _ _)  = error "mapActFree received a New."


-- | = Change the music

mapReplace :: Dispatch -> MuseqName -> Museq Action -> IO ()
mapReplace disp newName newMuseq = do
  masOld <- readMVar $ mMuseqs disp
  mapReplaceAll disp $ M.insert newName newMuseq masOld

-- | ASSUMES every synth has an "amp" parameter which, when 0, causes silence.
--
-- Strategy: Upon changing Museqs, compute which synths need deletion,
-- and which need creation. Create the latter immediately. Wait to delete
-- the former until it's safe, and before deleting them, send silence
-- (set "amp" to 0).
mapReplaceAll :: Dispatch -> M.Map MuseqName (Museq Action) -> IO ()
mapReplaceAll disp masNew = do
  time0  <-      takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  masOld <-      takeMVar $ mMuseqs      disp
  reg <-         takeMVar $ mReg         disp
  now <- unTimestamp <$> getTime

  let when = nextPhase0 time0 frameDuration now + 2 * frameDuration
        -- `when` = the start of the first not-yet-rendered frame
      toFree, toCreate :: [(SynthDefEnum, SynthName)]
      (toFree,toCreate) = mapMuseqsDiff masOld masNew

  newTransform  <- mapM (mapActNew  reg)      $ map (uncurry New)  toCreate
  freeTransform <- mapM (mapActFree reg when) $ map (uncurry Free) toFree

  putMVar (mTime0       disp) time0       -- unchnaged
  putMVar (mTempoPeriod disp) tempoPeriod -- unchanged
  putMVar (mMuseqs      disp) masNew
  putMVar (mReg         disp) $ foldl (.) id newTransform reg

  forkIO $ do wait $ when - now -- delete register's synths once it's safe
              reg <-takeMVar $ mReg disp
              putMVar (mReg disp) $ foldl (.) id freeTransform reg

  return ()

mapChTempoPeriod :: Dispatch -> Duration -> IO ()
mapChTempoPeriod disp newTempoPeriod = do
  time0       <- takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  now         <- unTimestamp <$> getTime
  let np0 = nextPhase0 time0 frameDuration now
      startRender = np0 + 2 * frameDuration
        -- np0 and startRender are defined similar to dispatchLoop,
        -- EXCEPT: multiply frameDuration by 2, because np0 is one period less
        -- than it will be the next time dispatchLoop runs
      startRenderInCycles = (startRender - time0) / tempoPeriod
      newTime0 = startRender - startRenderInCycles * newTempoPeriod
  putMVar (mTempoPeriod disp) newTempoPeriod
  putMVar (mTime0       disp) newTime0


-- | = The Dispatch loop

mapStartDispatchLoop :: Dispatch -> IO ThreadId
mapStartDispatchLoop disp = do
  tryTakeMVar $ mTime0 disp -- empty it, just in case
  mbTempo <- tryReadMVar $ mTempoPeriod disp
  maybe (putMVar (mTempoPeriod disp) 1) (const $ return ()) mbTempo
  
  (+(frameDuration * (-0.8))) . unTimestamp <$> getTime
    -- subtract nearly an entire frameDuration so it starts sooner
    >>= putMVar (mTime0 disp)
  forkIO $ mapDispatchLoop disp

mapDispatchLoop :: Dispatch -> IO ()
mapDispatchLoop disp = do
  time0  <-      takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  museqsMap <-   takeMVar $ mMuseqs      disp
  reg <-         takeMVar $ mReg         disp
  now <- unTimestamp <$> getTime

  let np0 = nextPhase0 time0 frameDuration now
      startRender = np0 + frameDuration
      evs = concatMap f $ M.elems museqsMap :: [(Time, Action)] where
        f :: Museq Action -> [(Time, Action)]
        f m = map (over _1 fst)  -- use `fst` to ignore message's end time
          $ arc time0 tempoPeriod startRender (startRender + frameDuration) m

  mapM_ (uncurry $ mapActSend reg) evs

  putMVar (mTime0       disp) time0
  putMVar (mTempoPeriod disp) tempoPeriod
  putMVar (mMuseqs      disp) museqsMap
  putMVar (mReg         disp) reg

  wait $ fromRational np0 - now
  mapDispatchLoop disp

showEvs evs = concatMap (\(t,a) -> "\n" ++ show t ++ ": " ++ show a) evs
