{-# LANGUAGE DataKinds
, ScopedTypeVariables #-}

module Montevideo.Monome.Types.Window (
    initAllWindows -- ^ MVar St -> [Window] -> IO ()
  , handleSwitch   -- ^ [Window] -> MVar St -> ((X,Y), Switch) -> IO ()

  -- | * only exported for the sake of testing
  , belongsHere    -- ^ [Window] -> Window -> LedFilter

-- | * So far, no need to export these.
--  LedRelay, LedFilter
--  , doSoundMessage -- ^ St -> SoundMsg -> IO ()
--  , doLedMessage   -- ^ St -> [Window] -> LedMsg -> IO ()
--  , relayToWindow  -- ^ St -> WindowId -> [Window] -> LedRelay
--  , relayIfHere    -- ^ Socket > [Window] -> Window -> LedRelay
--  , findWindow     -- ^ [Window] -> WindowId -> Maybe Window
  ) where

import           Prelude hiding (pred)
import           Control.Concurrent.MVar
import           Control.Lens hiding (set)
import qualified Data.List as L
import qualified Data.Map as M
import           Vivid hiding (pitch, synth, Param)

import Montevideo.Monome.Network.Util
import Montevideo.Synth.Boop_Monome
import Montevideo.Monome.Types.Button
import Montevideo.Monome.Types.Initial


-- | Forward a message to the monome if appropriate.
-- These are only used in this module.
type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = (X,Y) -> Bool

initAllWindows :: forall app. MVar (St app) -> IO ()
initAllWindows mst = do
  st <- readMVar mst
  let runWindowInit :: Window app -> IO ()
      runWindowInit w = let
        st' :: St app = windowInit w st
        in mapM_ (doLedMessage st') $ _stPending_Monome st'
  mapM_ runWindowInit $ _stWindowLayers st

-- | called every time a monome button is pressed or released
handleSwitch :: forall app.
                MVar (St app) -> ((X,Y), Switch) -> IO ()
handleSwitch    mst              sw@ (btn,_)      = do
  st0 <- takeMVar mst
  let go :: [Window app] -> IO ()
      go    []            = error $
        "handleSwitch: Switch " ++ show sw ++ " claimed by no Window."
      go    (w:ws)   =
        case windowContains w btn of
          True -> do
            let st1 = windowRoutine w st0 sw
            mapM_ (doSoundMessage st1) $ _stPending_Vivid  st1
            mapM_ (doLedMessage st1)   $ _stPending_Monome st1
            putMVar mst st1 { _stPending_Monome = []
                            , _stPending_Vivid = [] }
          False -> go ws
  go $ _stWindowLayers st0

doSoundMessage :: St app -> SoundMsg app -> IO ()
doSoundMessage    st        sdMsg         = do
  let vid   :: VoiceId = _soundMsgVoiceId sdMsg
      param :: Param   = _soundMsgParam   sdMsg
      f     :: Float   = _soundMsgVal     sdMsg
      v     :: Synth BoopParams =
        (_stVoices st M.! vid) ^. voiceSynth
  case param of
    "amp"  -> set v (toI f :: I "amp")
    "freq" -> set v (toI f :: I "freq")
    _      -> error $
      "doSoundMessage: unrecognized parameter " ++ param

doLedMessage :: St app -> LedMsg -> IO ()
doLedMessage st (l, (xy,b)) =
  let toWindow = relayToWindow st l
  in toWindow (xy,b)

relayToWindow :: St app -> WindowId -> LedRelay
relayToWindow st wl = let
  ws = _stWindowLayers st
  w = maybe err id $ findWindow ws wl
    where err = error $ "relayToWindow: " ++ wl ++ " not found."
  in relayIfHere (_stToMonome st) ws w

-- | `relayIfHere dest ws w` returns a `LedRelay` which,
-- if the coordinate falls in `w` and in no other `Window` before `w` in `ws`,
-- sends the message to the `Socket`.
relayIfHere :: Socket -> [Window app] -> Window app -> LedRelay
relayIfHere dest ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w $ fst msg
    then (send dest $ ledOsc "/monome" msg) >> return ()
    else return ()

-- | `belongsHere allWindows w _` returns a `Filter` that returns `True`
-- if `(X,Y)` belongs in `w` and none of the `Window`s preceding `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.
belongsHere :: [Window app] -> Window app -> LedFilter
belongsHere allWindows w = f where
  obscurers = takeWhile (/= w) allWindows
    -- `obscurers` == the windows above `w`
  obscured :: (X,Y) -> Bool
  obscured xy = or $ map ($ xy) $ map windowContains obscurers
  f :: (X,Y) -> Bool
  f btn = not (obscured btn) && windowContains w btn

findWindow :: [Window app] -> WindowId -> Maybe (Window app)
findWindow ws l = L.find pred ws where
  -- Pitfall: Assumes the window will be found.
  pred = (==) l . windowLabel
