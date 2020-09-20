{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Monome.Window.Util (
    LedRelay, LedFilter
  , nextVoice -- ^ M.Map VoiceId a -> VoiceId
  , relayToWindow -- ^ St -> WindowId -> [Window] -> LedRelay

  -- | * only exported for the sake of testing
  , relayIfHere   -- ^ Socket > [Window] -> Window -> LedRelay
  , belongsHere    -- ^ [Window] -> Window -> LedFilter
  ) where

import           Prelude hiding (pred)
import           Data.Either.Combinators
import qualified Data.List as L
import qualified Data.Map as M

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types
import Montevideo.Monome.Util.OSC


-- | Forward a message to the monome if appropriate.
type LedRelay  = ((X,Y), Led) -> IO ()
type LedFilter = (X,Y) -> Bool

nextVoice :: St a -> VoiceId
nextVoice st =
  case M.lookupMax $ _stVoices st of
    Nothing             -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1

relayToWindow :: St app -> MonomeId -> WindowId -> Either String LedRelay
relayToWindow st mi wl =
  mapLeft ("relayToWindow: " ++) $ do
  ws :: [Window app] <-
    maybe (Left $ "Wdindows for " ++ show mi ++ " not found.")
    Right $ M.lookup mi $ _stWindowLayers st
  w :: Window app <-
    maybe (Left $ "Window " ++ show wl ++ " not found.")
    Right $ L.find ((==) wl . windowLabel) ws
  sock :: Socket <-
    maybe (Left $ "Relay to " ++ show mi ++ " not found.")
    Right $ M.lookup mi $ _stToMonome st
  Right $ relayIfHere (sock, mi) ws w


-- * only exported for the sake of testing

-- | `relayIfHere dest ws w` returns a `LedRelay` which,
-- if the coordinate falls in `w` and in no other `Window` before `w` in `ws`,
-- sends the message to the `Socket`.
relayIfHere :: (Socket, MonomeId) -> [Window app] -> Window app -> LedRelay
relayIfHere (dest, mi) ws w = f where
  f :: ((X,Y),Led) -> IO ()
  f msg = if belongsHere ws w $ fst msg
          then do send dest $ ledOsc mi msg
                  return ()
          else return ()

-- | `belongsHere allWindows w _` returns an `LedFilter` that returns `True`
-- if `(X,Y)` belongs in `w` and none of the `Window`s preceding `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.
belongsHere :: [Window app] -> Window app -> LedFilter
belongsHere allWindows w = f where
  higherWindows = takeWhile (/= w) allWindows
  obscured :: (X,Y) -> Bool
  obscured xy = or $
    map (($ xy) . windowContains) higherWindows
  f :: (X,Y) -> Bool
  f btn = not (obscured btn) && windowContains w btn
