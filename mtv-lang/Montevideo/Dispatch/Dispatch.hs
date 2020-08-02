{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Dispatch (
  -- | = change the music
    stop_inDisp       -- ^ Dispatch -> MuseqName -> IO ()
  , replace_inDisp    -- ^ Dispatch -> MuseqName -> Museq String Note -> IO ()
  , replaceAll_inDisp -- ^ Dispatch -> M.Map MuseqName (Museq String Note) -> IO ()
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

-- | Stop one of the `Museq`s
stop_inDisp :: Dispatch -> MuseqName -> IO ()
stop_inDisp disp name = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll_inDisp disp $ M.delete name masOld

-- | Replace one of the `Museq`s
replace_inDisp :: Dispatch -> MuseqName -> Museq String Note -> IO ()
replace_inDisp disp newName newMuseq = do
  masOld <- readMVar $ mMuseqs disp
  replaceAll_inDisp disp $ M.insert newName newMuseq masOld


-- | ASSUMES every synth has an "amp" parameter which, when 0, causes silence.
--
-- Strategy: Upon changing Museqs, compute which synths need deletion,
-- and which need creation. Create the latter immediately. Wait to delete
-- the former until it's safe (half a `frameDuration`),
-- and before deleting them, send silence
-- (set "amp" to 0).
--
-- This computes the next thing to play in parallel, rather than
-- halting SuperCollider communication until the next thing is computed.

replaceAll_inDisp :: Dispatch -- ^ What plays stuff.
           -> M.Map MuseqName (Museq String Note) -- ^ What to play next.
           -> IO ()
replaceAll_inDisp disp mqsNew = do
  time0  <- readMVar $ mTime0  disp
  mqsOld <- readMVar $ mMuseqs disp
  reg    <- readMVar $ mReg    disp
  now    <- unTimestamp <$> getTime

  let
    -- Append the name of each Museq to each of its events.
    mqsNew' = M.mapWithKey f mqsNew where
      f :: MuseqName -> Museq String Note
                     -> Museq String Note
      f name = vec %~ (V.map $ evLabel %~ (name ++))
        -- This guarantees different Museqs in the map will not conflict --
        -- and that they cannot cooperate in the same synth.

    when = nextPhase0 time0 frameDuration now + frameDuration
      -- `when` = the end of the first not-yet-rendered frame.
      -- TODO (speed) ? Is this conservative? Do I not need to
      -- `(+ frameDuration)`?
    toFree, toCreate :: [(SynthDefEnum, SynthName)]
    (toFree,toCreate) = museqSynthsDiff mqsOld mqsNew'

  newTransform  :: [SynthRegister -> SynthRegister] <-
    mapM (dispatchConsumeScAction_New  reg)      $
    map (uncurry ScAction_New)  toCreate
  freeTransform :: [SynthRegister -> SynthRegister] <-
    mapM (dispatchConsumeScAction_Free reg when) $
    map (uncurry ScAction_Free) toFree

  let synthRegisterNew :: SynthRegister =
        reg &
        (foldl (.) id newTransform :: SynthRegister -> SynthRegister)

  -- Make sure everything we need is calculated,
  -- then empty all the MVars and replace them with this new stuff.
  seq (mqsNew', synthRegisterNew) $ return ()
  _ <- takeMVar $ mMuseqs disp
  _ <- takeMVar $ mReg    disp
  putMVar (mMuseqs        disp) mqsNew'
  putMVar (mReg           disp) synthRegisterNew

  _ <- forkIO $ do
    wait $ when - now -- delete register's synths once it's safe
    reg1 <- takeMVar $ mReg disp
    putMVar (mReg disp) $ foldl (.) id freeTransform reg1

  return ()

-- | Changing tempo requires changing time0, too,
-- to avoid changing which cycle one is currently in.
-- For instance, if time0 = 0, tempoDuration = 1, and it's time 10,
-- then we're in the 10th cycle. If we change tempoDuration to be 0.5,
-- we need to change time0 from 0 to 5;
-- otherwise we'll suddenly be in the 20th cycle.
chTempoPeriod :: Dispatch
              -> Duration -- ^ period (not frequency) of the new tempo
              -> IO ()

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
  -- set `mTime0 disp`
  _ <- tryTakeMVar $ mTime0 disp -- empty it, just in case
  (-) (0.8 * frameDuration) . unTimestamp <$> getTime
    -- subtract nearly an entire frameDuration so it starts soon
    >>= putMVar (mTime0 disp)

  -- set `mTempoPeriod disp` to 1 if it's unset
  _ <- tryPutMVar (mTempoPeriod disp) 1

  -- set `mReg disp`
  buffs :: M.Map Sample BufferId <-
    mapM newBufferFromFile $ samplePaths
  _ <- tryTakeMVar $ mReg disp -- empty it, just in case
  putMVar (mReg disp) $
    emptySynthRegister & samples .~ buffs

  forkIO $ dispatchLoop disp

dispatchLoop :: Dispatch -> IO ()
dispatchLoop disp = do
  time0       <- takeMVar $ mTime0       disp
  tempoPeriod <- takeMVar $ mTempoPeriod disp
  notes :: M.Map MuseqName (Museq String Note)
              <- takeMVar $ mMuseqs      disp
  reg         <- takeMVar $ mReg         disp
  now         <- unTimestamp <$> getTime

  let
    start :: Time = nextPhase0 time0 frameDuration now
    actions :: M.Map String (Museq String (ScAction SynthName)) =
      museq_NotesToScActions notes
    evs0 :: [(Time, ScAction SynthName)] =
      concatMap (museqFrame time0 tempoPeriod start)
      $ M.elems actions

  mapM_ (uncurry $ dispatchConsumeScAction_Send reg) evs0

  putMVar (mTime0       disp) time0
  putMVar (mTempoPeriod disp) tempoPeriod
  putMVar (mMuseqs      disp) notes
  putMVar (mReg         disp) reg

  wait $ fromRational start - now
  dispatchLoop disp
