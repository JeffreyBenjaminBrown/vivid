{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vivid.Jbb.Dispatch.Dispatch (
  -- | = mess with a SynthRegister
      act   -- ^ SynthRegister -> Time -> Action -> IO (SynthRegister -> SynthRegister)
  , actNew  -- ^ SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
  , actFree -- ^ SynthRegister -> Rational -> Action -> IO (SynthRegister -> SynthRegister)
  , actSend -- ^ SynthRegister -> Time -> Action -> IO ()

  -- | = change the music
  , stopDispatch       -- ^ Dispatch -> MuseqName -> IO ()
  , replace'    -- ^ Dispatch -> MuseqName -> Museq String Note -> IO ()
  , replaceAll' -- ^ Dispatch -> M.Map MuseqName (Museq String Note) -> IO ()
  , chTempoPeriod'     -- ^ Dispatch -> Duration -> IO ()

  -- | = the dispatch loop
  , startDispatchLoop' -- ^ Dispatch -> IO ThreadId
  , dispatchLoop'      -- ^ Dispatch -> IO ()
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Lens (set, over, _1, _2, (^.))
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid hiding (set, when)
import Vivid.Jbb.Dispatch.Config (frameDuration)
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Synths
import Vivid.Jbb.Util


-- | = Act on a Msg

-- todo ? `act` might never get used
act :: SynthRegister -> Time -> Action
     -> IO (SynthRegister -> SynthRegister)
act reg t a@(Send _ _ _) = actSend reg t a >> return id
act reg t a@(Free _ _)   = actFree reg t a
act reg _ a@(New _ _)    = actNew  reg   a


actNew :: SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
actNew reg (New Boop name) = case M.lookup name $ _boops reg of
    Nothing -> do s <- synth boop ()
                  return $ over boops $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id

actNew reg (New Vap name) = case M.lookup name $ _vaps reg of
    Nothing -> do s <- synth vap ()
                  return $ over vaps $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id

actNew reg (New Sqfm name) = case M.lookup name $ _sqfms reg of
    Nothing -> do s <- synth sqfm ()
                  return $ over sqfms $ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id

actNew _ (Send _ _ _) = error $ "actNew received a Send."
actNew _ (Free _ _)   = error $ "actNew received a Free."


actFree :: SynthRegister -> Rational -> Action -> IO (SynthRegister -> SynthRegister)
actFree reg when (Free Boop name) = case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> do
    doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
    doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
    return $ over boops $ M.delete name

actFree reg when (Free Vap name) = case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> do
    doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
    doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
    return $ over vaps $ M.delete name

actFree reg when (Free Sqfm name) = case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> do
    doScheduledAt (Timestamp $ fr when) $ set' s $ Msg' (0 :: I "amp")
    doScheduledAt (Timestamp $ fr $ when + frameDuration / 2) $ free s
    return $ over sqfms $ M.delete name

actFree _ _ (Send _ _ _) = error "actFree received a Send."
actFree _ _ (New _ _)    = error "actFree received a New."


actSend :: SynthRegister -> Time -> Action -> IO ()
actSend reg when (Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: Synth BoopParams) ->
      doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' s) $ boopMsg msg

actSend reg when (Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: Synth SqfmParams) ->
      doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' s) $ sqfmMsg msg

actSend reg when (Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: Synth VapParams) ->
      doScheduledAt (Timestamp $ fromRational when)
      $ mapM_ (set' s) $ vapMsg msg

actSend _ _ (Free _ _) = error "actFree received a Send."
actSend _ _ (New _ _)  = error "actFree received a New."


-- | = Change the music

stopDispatch :: Dispatch -> MuseqName -> IO ()
stopDispatch disp name = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll' disp $ M.delete name masOld

replace' :: Dispatch -> MuseqName -> Museq String Note -> IO ()
replace' disp newName newMuseq = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll' disp $ M.insert newName newMuseq masOld


-- | ASSUMES every synth has an "amp" parameter which, when 0, causes silence.
--
-- Strategy: Upon changing Museqs, compute which synths need deletion,
-- and which need creation. Create the latter immediately. Wait to delete
-- the former until it's safe, and before deleting them, send silence
-- (set "amp" to 0).

replaceAll' :: Dispatch -> M.Map MuseqName (Museq String Note) -> IO ()
replaceAll' disp masNew = do
  time0  <-      takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  masOld <-      takeMVar $ mMuseqs      disp
  reg <-         takeMVar $ mReg         disp
  now <- unTimestamp <$> getTime

  let masNew' = M.mapWithKey f masNew where
        f :: MuseqName -> Museq String Note -> Museq String Note
        f museqName m = over vec (V.map $ over evLabel (museqName ++)) m
          -- including the MuseqName guarantees different Museqs in the map
          -- will not conflict (and cannot cooperate in the same synth)
      when = nextPhase0 time0 frameDuration now + frameDuration
        -- `when` = the start of the first not-yet-rendered frame
      toFree, toCreate :: [(SynthDefEnum, SynthName)]
      (toFree,toCreate) = museqsDiff' masOld masNew'

  newTransform  <- mapM (actNew  reg)      $ map (uncurry New)  toCreate
  freeTransform <- mapM (actFree reg when) $ map (uncurry Free) toFree

  putMVar (mTime0       disp) time0       -- unchnaged
  putMVar (mTempoPeriod disp) tempoPeriod -- unchanged
  putMVar (mMuseqs      disp) masNew'
  putMVar (mReg         disp) $ foldl (.) id newTransform reg

  _ <- forkIO $ do
    wait $ when - now -- delete register's synths once it's safe
    reg1 <-takeMVar $ mReg disp
    putMVar (mReg disp) $ foldl (.) id freeTransform reg1

  return ()

chTempoPeriod' :: Dispatch -> Duration -> IO ()
chTempoPeriod' disp newTempoPeriod = do
  time0       <- takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  now         <- unTimestamp <$> getTime
  let when = nextPhase0 time0 frameDuration now + frameDuration
        -- `when` here is defined similar to `when` in `dispatchLoop`,
        -- EXCEPT: add `frameDuration`, because `when` is one period
        -- less than it will be the next time `dispatchLoop` runs
      whenInCycles = (when - time0) / tempoPeriod
      newTime0 = when - whenInCycles * newTempoPeriod
  putMVar (mTempoPeriod disp) newTempoPeriod
  putMVar (mTime0       disp) newTime0


-- | = The Dispatch loop
startDispatchLoop' :: Dispatch -> IO ThreadId
startDispatchLoop' disp = do
  _ <- tryTakeMVar $ mTime0 disp -- empty it, just in case
  mbTempo <- tryReadMVar $ mTempoPeriod disp
  maybe (putMVar (mTempoPeriod disp) 1) (const $ return ()) mbTempo

  ((-) (0.8 * frameDuration)) . unTimestamp <$> getTime
    -- subtract nearly an entire frameDuration so it starts soon
    >>= putMVar (mTime0 disp)
  forkIO $ dispatchLoop' disp

dispatchLoop' :: Dispatch -> IO ()
dispatchLoop' disp = do
  time0  <-      takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  museqsMap <-   takeMVar $ mMuseqs      disp
  reg <-         takeMVar $ mReg         disp
  now <- unTimestamp <$> getTime

  let
    startRender = nextPhase0 time0 frameDuration now
    museqsMap' = M.map g museqsMap :: M.Map String(Museq String Action) where
      g museq = over vec (V.map f) museq where
        f :: Ev String Note -> Ev String Action
          -- todo ? awkward : The Ev label gets repeated within the Action. 
        f ev = let note = ev ^. evData
                   ac = Send (note^.noteSd) (ev^.evLabel) (note^.noteMsg)
               in set evData ac ev

    evs0 = concatMap f $ M.elems museqsMap' :: [(Time, Action)] where
      f :: Museq String Action -> [(Time, Action)] -- start times and actions
      f m = map (\ev -> ((ev^.evStart), (ev^.evData))) evs
        where evs = arc' time0 tempoPeriod startRender
                    (startRender + frameDuration) m

  mapM_ (uncurry $ actSend reg) evs0

  putMVar (mTime0       disp) time0
  putMVar (mTempoPeriod disp) tempoPeriod
  putMVar (mMuseqs      disp) museqsMap
  putMVar (mReg         disp) reg

  wait $ fromRational startRender - now
  dispatchLoop' disp
