{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Monome.Window.Util (
    LedRelay, LedFilter
  , nextVoice     -- ^ M.Map VoiceId a -> VoiceId
  , relayToWindow -- ^ St app -> MonomeId -> WindowId
                  -- -> Either String LedRelay

  -- | * only exported for the sake of testing
  , relayIfHere
  , visible
  ) where

import           Prelude hiding (pred)
-- import           Control.Lens
import           Data.Either.Combinators
import qualified Data.List as L
import qualified Data.Map as M

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types
import Montevideo.Monome.Util.OSC
import Montevideo.Util


-- | Forward a message to the monome if appropriate.
type LedRelay  = LedMsg -> IO ()
type LedFilter = (X,Y) -> Bool

nextVoice :: St a -> VoiceId
nextVoice st =
  case M.lookupMax $ _stVoices st of
    Nothing             -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1

relayToWindow :: forall app.
  St app -> MonomeId -> WindowId -> Either String LedRelay
relayToWindow st mi wl =
  mapLeft ("relayToWindow: " ++) $ do
  ws :: [((X,Y), Window app)] <-
    maybe (Left $ "Wdindows for " ++ show mi ++ " not found.")
    Right $ M.lookup mi $ _stWindowLayers st
  w :: ((X,Y), Window app) <-
    maybe (Left $ "Window " ++ show wl ++ " not found.")
    Right $ L.find ((==) wl . windowLabel . snd) ws
  sock :: Socket <-
    maybe (Left $ "Relay to " ++ show mi ++ " not found.")
    Right $ M.lookup mi $ _stToMonomes st
  Right $ relayIfHere sock ws w


-- * only exported for the sake of testing

-- | `relayIfHere dest ws w` returns a `LedRelay` which,
-- if the coordinate falls in `w` and in no other `Window` before `w` in `ws`,
-- sends the message to the `Socket`.
--
-- PITFALL: The (X,Y) of the message received by the LedRelay
-- is assumed to be relative to the window `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.

relayIfHere :: Socket
  -> [((X,Y), Window app)] -- ^ All windows, in order, with top-left corners.
  -> ((X,Y), Window app)   -- ^ The Window onto which we might draw, and its top-left corner.
  -> LedRelay
relayIfHere dest allWindows w = f where
  f :: ((MonomeId, WindowId), ((X,Y), Led)) -> IO ()
  f ((mi,_), (xy,led)) = -- the WindowId is no longer needed, given w
    if visible allWindows w xy
    then do _ <- send dest $ ledOsc mi
                 ( pairAdd xy $ fst w -- absolute monome coordinates
                 , led )
            return ()
    else return () -- do nothing

-- | `visible allWindows w _` returns an `LedFilter` that returns `True`
-- if `(X,Y)` belongs in `w` and none of the `Window`s preceding `w`.
--
-- PITFALL: The LedMsg's (X,Y) coordinates are relative to `w`.
-- PITFALL: `allWindows` should include literally all of them, even `w`.

visible :: forall app.
     [((X,Y), Window app)] -- ^ All Windows, in order, with top-left corners.
  ->  ((X,Y), Window app)  -- ^ The Window onto which we might draw.
  -> LedFilter
visible allWindows (wtl, w) = f where
  f :: (X,Y) -> Bool
  f led = not (obscured led) && windowContains w led
  obscured :: (X,Y) -- the Led, relative to w
           -> Bool
  obscured led = or $ map (obscures led) higherWindows
  obscures :: (X,Y) -- the Led, relative to w
           -> ((X,Y), Window app) -- a Window that might block the Led
           -> Bool
  obscures wRelative (wtl', w') = let
    absolute = pairAdd wRelative wtl
    w'Relative = pairSubtract absolute wtl' -- the Led, relative to w'
    in windowContains w' w'Relative
  higherWindows :: [((X,Y), Window app)] = -- Windows that might block the Led
    takeWhile ((/= w) . snd) allWindows
