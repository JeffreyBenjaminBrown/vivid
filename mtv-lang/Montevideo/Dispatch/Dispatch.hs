{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Dispatch (
 -- | = change the music
    stopDispatch       -- ^ Dispatch -> MuseqName -> IO ()
  , replace    -- ^ Dispatch -> MuseqName -> Museq String Note -> IO ()
  , replaceAll -- ^ Dispatch -> M.Map MuseqName (Museq String Note) -> IO ()
  , chTempoPeriod     -- ^ Dispatch -> Duration -> IO ()

  -- | = the dispatch loop
  , startDispatchLoop -- ^ Dispatch -> IO ThreadId
  , dispatchLoop      -- ^ Dispatch -> IO ()
  ) where

import           Control.Concurrent (forkIO, ThreadId)
import           Control.Concurrent.MVar
import           Control.Lens hiding (set')
import qualified Data.Map as M
import qualified Data.Vector as V
import           Vivid hiding (set, when)

import Montevideo.Dispatch.Config (frameDuration)
import Montevideo.Dispatch.Msg.Act
import Montevideo.Dispatch.Museq
import Montevideo.Dispatch.Types
import Montevideo.Dispatch.Time
import Montevideo.Synth
import Montevideo.Synth.Samples


-- | = Change the music

stopDispatch :: Dispatch -> MuseqName -> IO ()
stopDispatch disp name = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll disp $ M.delete name masOld

replace :: Dispatch -> MuseqName -> Museq String Note -> IO ()
replace disp newName newMuseq = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll disp $ M.insert newName newMuseq masOld


-- | ASSUMES every synth has an "amp" parameter which, when 0, causes silence.
--
-- Strategy: Upon changing Museqs, compute which synths need deletion,
-- and which need creation. Create the latter immediately. Wait to delete
-- the former until it's safe, and before deleting them, send silence
-- (set "amp" to 0).
--
-- This computes the next thing to play in parallel, rather than
-- halting SuperCollider communication until the next thing is computed.

replaceAll :: Dispatch -> M.Map MuseqName (Museq String Note) -> IO ()
replaceAll disp masNew = do
  time0  <-      readMVar $ mTime0       disp
  masOld <-      readMVar $ mMuseqs      disp
  reg <-         readMVar $ mReg         disp
  now <- unTimestamp <$> getTime

  let masNew' = M.mapWithKey f masNew where
        f :: MuseqName -> Museq String Note
                       -> Museq String Note
        f name = vec %~ (V.map $ evLabel %~ (name ++))
          -- including the MuseqName guarantees different Museqs in the map
          -- will not conflict (and cannot cooperate in the same synth)
      when = nextPhase0 time0 frameDuration now + frameDuration
        -- `when` = the start of the first not-yet-rendered frame
      toFree, toCreate :: [(SynthDefEnum, SynthName)]
      (toFree,toCreate) = museqSynthsDiff masOld masNew'

  newTransform  <- mapM (actNew  reg)      $ map (uncurry New)  toCreate
  freeTransform <- mapM (actFree reg when) $ map (uncurry Free) toFree

  let synthRegisterNew = foldl (.) id newTransform reg

  -- make sure everything we need is calculated,
  -- then empty all the MVars and replace them with this new stuff
  seq (masNew', synthRegisterNew) $ return ()
  _ <- takeMVar $ mMuseqs      disp
  _ <- takeMVar $ mReg         disp
  putMVar (mMuseqs      disp) masNew'
  putMVar (mReg         disp) synthRegisterNew

  _ <- forkIO $ do
    wait $ when - now -- delete register's synths once it's safe
    reg1 <-takeMVar $ mReg disp
    putMVar (mReg disp) $ foldl (.) id freeTransform reg1

  return ()

chTempoPeriod :: Dispatch -> Duration -> IO ()
chTempoPeriod disp newTempoPeriod = do
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
startDispatchLoop :: Dispatch -> IO ThreadId
startDispatchLoop disp = do
  _       <- tryTakeMVar $ mTime0       disp -- empty it, just in case

  mbTempo <- tryReadMVar $ mTempoPeriod disp
  case mbTempo of Nothing -> putMVar (mTempoPeriod disp) 1
                  Just _ -> return ()

  _       <- tryTakeMVar $ mReg         disp -- empty it, just in case
  buffs :: M.Map Sample BufferId <-
    mapM newBufferFromFile $ samplePaths
  putMVar (mReg disp) $
    SynthRegister mempty mempty mempty buffs mempty mempty

  ((-) (0.8 * frameDuration)) . unTimestamp <$> getTime
  -- subtract nearly an entire frameDuration so it starts soon
    >>= putMVar (mTime0 disp)
  forkIO $ dispatchLoop disp

dispatchLoop :: Dispatch -> IO ()
dispatchLoop disp = do
  time0       <- takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  museqsMap   <- takeMVar $ mMuseqs      disp
  reg         <- takeMVar $ mReg         disp
  now         <- unTimestamp <$> getTime

  let
    startRender = nextPhase0 time0 frameDuration now
    museqsMap' :: M.Map String(Museq String Action)
      = M.map g museqsMap where
      g :: Museq String Note -> Museq String Action
        = vec %~ V.map f where
        f :: Ev String Note -> Ev String Action
        -- todo ? awkward : Ev label is repeated in Action.
        f ev = evData .~ ac $ ev where
          d = ev ^. evData
          ac = Send (d^.noteSd) (ev^.evLabel) (d^.noteMsg)

    evs0 :: [(Time, Action)]
      = concatMap f $ M.elems museqsMap' where
      f :: Museq String Action
        -> [(Time, Action)] -- start times and actions
      f m = map (\ev -> ((ev^.evStart), (ev^.evData))) evs
        where evs = arc time0 tempoPeriod startRender
                    (startRender + frameDuration) m

  mapM_ (uncurry $ actSend reg) evs0

  putMVar (mTime0       disp) time0
  putMVar (mTempoPeriod disp) tempoPeriod
  putMVar (mMuseqs      disp) museqsMap
  putMVar (mReg         disp) reg

  wait $ fromRational startRender - now
  dispatchLoop disp
