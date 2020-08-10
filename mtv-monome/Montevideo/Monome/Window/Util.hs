{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Monome.Window.Util (
  -- | * only exported for the sake of testing
  belongsHere    -- ^ [Window] -> Window -> LedFilter

  , LedRelay, LedFilter
  , relayToWindow -- ^ St -> WindowId -> [Window] -> LedRelay
  , relayIfHere   -- ^ Socket > [Window] -> Window -> LedRelay
  , findWindow    -- ^ [Window] -> WindowId -> Maybe Window

  , nextVoice -- ^ M.Map VoiceId a -> VoiceId
  ) where

import           Prelude hiding (pred)
import           Data.Either.Combinators
import qualified Data.List as L
import qualified Data.Map as M

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types
import Montevideo.Monome.Util.Button


-- | Forward a message to the monome if appropriate.
-- These are only used in this module.
type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = (X,Y) -> Bool

relayToWindow :: St app -> WindowId -> Either String LedRelay
relayToWindow st wl =
  mapLeft ("relayToWindow: " ++) $ do
  let ws = _stWindowLayers st
  w <- maybe (Left $ "relayToWindow: " ++ wl ++ " not found.")
       Right $ findWindow ws wl
  Right $ relayIfHere (_stToMonome st) ws w

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

nextVoice :: St a -> VoiceId
nextVoice st =
  case M.lookupMax $ _stVoices st of
    Nothing -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1
    -- Note that (0,1) < (1,0).
