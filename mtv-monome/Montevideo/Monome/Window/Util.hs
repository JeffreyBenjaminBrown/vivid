{-# LANGUAGE DataKinds
, ScopedTypeVariables #-}

module Montevideo.Monome.Window.Util (
    initAllWindows -- ^ MVar St -> [Window] -> IO ()
  , handleSwitch -- ^ MVar (St app) -> ((X,Y), Switch)
                 -- -> IO (Either String ())

  -- | * only exported for the sake of testing
  , belongsHere    -- ^ [Window] -> Window -> LedFilter

-- | * So far, no need to export these.
--  , LedRelay, LedFilter
--  , doScAction    -- ^ St -> ScAction VoiceId -> IO ()
--  , doLedMessage  -- ^ St -> [Window] -> LedMsg -> IO ()
--  , relayToWindow -- ^ St -> WindowId -> [Window] -> LedRelay
--  , relayIfHere   -- ^ Socket > [Window] -> Window -> LedRelay
--  , findWindow    -- ^ [Window] -> WindowId -> Maybe Window

  , nextVoice -- ^ M.Map VoiceId a -> VoiceId
  ) where

import           Prelude hiding (pred)
import           Control.Concurrent.MVar
import           Control.Lens hiding (set)
import           Data.Either.Combinators
import qualified Data.List as L
import qualified Data.Map as M
import qualified Vivid as V

import Montevideo.Dispatch.Types.Many
import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types
import Montevideo.Monome.Util.Button
import Montevideo.Synth


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
        in mapM_ (either putStrLn id) $
           doLedMessage st' <$> _stPending_Monome st'
  mapM_ runWindowInit $ _stWindowLayers st

-- | Called every time a monome button is pressed or released.
-- Does two kinds of IO: talking to SuperCollider and changing the MVar.
-- TODO : instead of MVar IO, return an St -> St.
-- (That way I could test it.)
handleSwitch :: forall app.
                MVar (St app) -> ((X,Y), Switch) -> IO (Either String ())
-- PITFALL: The order of execution in `handleSwitch` is complex.
-- `windowHandler` runs before `doScAction` and `doLedMessage`.
-- Thus some updates to the St (everything but voice parameters,
-- the `voiceSynth` field, and voice deletion)
-- happen before SC has been informed.

handleSwitch mst sw@ (btn,_) = do
  st0 <- takeMVar mst
  let
    go :: [Window app] -> IO (Either String ())
    go [] = return $ Left $
     "Switch " ++ show sw ++ " claimed by no Window."
    go (w:ws) = case windowContains w btn of
      False -> go ws
      True -> case windowHandler w st0 sw of
        Left s -> return $ Left s
        Right st1 -> do

          mapM_ (either putStrLn id) $
            doLedMessage st1 <$> _stPending_Monome st1
          let eioefs :: Either String [IO ( Either String
                                            (St app -> St app ))] =
                mapM (doScAction st1) (_stPending_Vivid  st1)
          case eioefs of
            Left s -> return $ Left s
            Right (ioefs :: [IO (Either String (St app -> St app))]) -> do

              efs :: [Either String (St app -> St app)] <-
                mapM id ioefs
              case mapM id efs :: Either String [St app -> St app] of
                Left s -> return $ Left s
                Right fs -> do
                  putMVar mst (foldl (.) id fs st1)
                    { _stPending_Monome = []
                    , _stPending_Vivid = [] }
                  return $ Right ()

  fmap (mapLeft ("Window.Util.handleSwitch: " ++)) $
    go $ _stWindowLayers st0

-- | PITFALL: The order of execution here is kind of strange.
-- See comments in `handleSwitch` for details.
-- TODO ? Could the outer Either be stripped from this function?
doScAction :: St app -> ScAction VoiceId
           -> Either String (IO (Either String (St a -> St a)))
doScAction    st        sca =
  mapLeft ("doScAction: " ++) $
  case sca of

    ScAction_Send _ _ _ -> do -- currently unused
      let vid :: VoiceId = _actionSynthName sca
      s :: V.Synth MoopParams <-
        maybe (Left $ "VoiceId " ++ show vid ++ " has no assigned synth.")
        Right $ (_stVoices st M.! vid) ^. voiceSynth
      let go :: (ParamName, Float) -> Either String (IO ())
          go (param, f) =
             case param of
               "amp"  -> Right $ V.set s (V.toI f :: V.I "amp")
               "freq" -> Right $ V.set s (V.toI f :: V.I "freq")
               _      -> Left $ "unrecognized parameter " ++ param
      ios :: [IO ()] <- mapM go $ M.toList $ _actionScMsg sca
      Right $ mapM_ id ios >> return (Right id)

    ScAction_New _ _ _ -> do
      let vid :: VoiceId = _actionSynthName sca
      _ <- maybe (Left $ "VoiceId " ++ show vid ++ " should be present "
                  ++ " (but not yet with a synth) in _stVoices.")
           Right $ M.lookup vid $ _stVoices st
      Right $ do
        s <- V.synth moop () -- TODO change moop to `_actionSynthDefEnum sca`
        let go :: (ParamName, Float) -> Either String (IO ())
            go (param, f) =
               case param of
                 "amp"  -> Right $ V.set s (V.toI f :: V.I "amp")
                 "freq" -> Right $ V.set s (V.toI f :: V.I "freq")
                 _      -> Left $ "unrecognized parameter " ++ param
        case mapM go $ M.toList $ _actionScMsg sca of
          Left err -> return $ Left err
          Right (ios :: [IO ()]) -> do
            mapM_ id ios
            return $ Right $
              stVoices . at vid . _Just . voiceSynth .~ Just s

    ScAction_Free _ _ -> do
      let vid :: VoiceId = _actionSynthName sca
      v :: Voice a <- maybe
        (Left $ "VoiceId " ++ show vid ++ " absent from _stVoices.")
        Right $ M.lookup vid $ _stVoices st
      s :: V.Synth MoopParams <- maybe
        (Left $ "Voice " ++ show vid ++ " has no assigned synth.")
        Right $ _voiceSynth v
      Right $ do
        V.free s
        return $ Right $ stVoices . at vid .~ Nothing

doLedMessage :: St app -> LedMsg -> Either String (IO ())
doLedMessage st (l, (xy,b)) =
  mapLeft ("doLedMessage: " ++) $
  case relayToWindow st l of
    Left s         -> Left s
    Right toWindow -> Right $ toWindow (xy,b)

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

nextVoice :: M.Map VoiceId a -> VoiceId
nextVoice m =
  case M.lookupMax m of
    Nothing -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1
    -- Note that (0,1) < (1,0).
