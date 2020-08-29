{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase
, OverloadedStrings
, DataKinds
, ScopedTypeVariables #-}

module Montevideo.Monome.Main (
  -- ** The apps
    edoMonome
  , jiMonome

  -- ** Utilities they use
  , renameMonomes -- ^ IO ()
  , initAllWindows -- ^ MVar St -> [Window] -> IO ()
  , handleSwitch -- ^ MVar (St app) -> ((X,Y), Switch)
                 -- -> IO (Either String ())

  -- * No need so far to export these. (No way to test them.)
  --  , doScAction    -- ^ St -> ScAction VoiceId -> IO ()
  --  , doLedMessage  -- ^ St -> [Window] -> LedMsg -> IO ()
  ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Lens hiding (set)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Either.Combinators
import qualified Data.Map as M

import Vivid
import Vivid.OSC

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Dispatch.Types.Time (unTimestamp)
import qualified Montevideo.Monome.Config.Mtv as Config
import           Montevideo.Monome.Network.Util
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Util.OSC
import           Montevideo.Monome.Window.JI
import           Montevideo.Monome.Window.Keyboard
import           Montevideo.Monome.Window.Lag
import           Montevideo.Monome.Window.Shift
import           Montevideo.Monome.Window.Sustain
import           Montevideo.Monome.Window.Util
import           Montevideo.Synth


-- ** The apps

edoMonome :: EdoConfig -> IO (St EdoApp)
edoMonome edoCfg = do
  renameMonomes
  inbox :: Socket <- receivesAt "127.0.0.1" 8000
    -- I don't know why it's port 8000, or why it used to be 11111.
  to256 :: Socket <- sendsTo (unpack localhost) 15226
  to128 :: Socket <- sendsTo (unpack localhost) 14336
    -- to find the port number above, use the first part of HandTest.hs

  mst <- newMVar $ St {
      _stWindowLayers = M.fromList
        [ ( Monome_256
          , [sustainWindow, shiftWindow, keyboardWindow] )
        , ( Monome_128
          , [lagWindow, keyboardWindow] ) ]
    , _stToMonome = M.fromList [ (Monome_256, to256)
                               , (Monome_128, to128) ]
    , _stVoices = mempty
    , _stPending_Vivid = []
    , _stPending_Monome = []
    , _stLag = 0.03

    , _stApp = EdoApp
        { _edoConfig = edoCfg
        , _edoXyShift = (0,0)
        , _edoFingers = mempty
        , _edoLit = mempty
          -- M.singleton (2 :: PitchClass) $ S.singleton LedBecauseAnchor
        , _edoSustaineded = mempty
        }
    }

  initAllWindows mst

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> do
        case readOSC_asSwitch osc of
          Left s -> putStrLn s
          Right (monome,switch) ->
            handleSwitch mst monome switch >>=
            either putStrLn return

  let loop :: IO (St EdoApp) =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          killThread responder
          st <- readMVar mst
          let f = maybe (putStrLn "voice with no synth") free
            in mapM_ (f . (^. voiceSynth)) (M.elems $ _stVoices st)
          _ <- let off receiver name = send receiver $ allLedOsc name False
               in off to256 Monome_256 >>
                  off to128 Monome_128
          return st
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop

-- | One way to make a major scale it to use
-- the generators [1,4/3,3/2] and [1,5/4,3/2].
-- (Another would be to use the generators [1] and
-- [1,9/8,5/4,4/3,3/2,5/3,15/8], but that's harder to play,
-- and its geometry gives no insight into the scale.)
jiMonome :: Int        -- ^ The monome address, as reported by serialoscd.
         -> [Rational] -- ^ The horizontal grid generator.
         -> [Rational] -- ^ The vertical grid generator.
         -> IO (St JiApp)
jiMonome monomePort scale shifts = do
  -- PITFALL: Every comment written in edoMonome also applies here.

  renameMonomes
  inbox :: Socket <- receivesAt "127.0.0.1" 8000
  toMonome :: Socket <- sendsTo (unpack localhost) monomePort

  mst <- newMVar $ St {
      _stWindowLayers = M.singleton Monome_256 [jiWindow]
    , _stToMonome     = M.singleton Monome_256 toMonome
    , _stVoices = mempty
    , _stPending_Vivid = []
    , _stPending_Monome = []
    , _stLag = 0.03

    , _stApp = JiApp { _jiGenerator = scale
                     , _jiShifts = shifts
                     , _jiFingers = mempty }
    }
  initAllWindows mst

  responder <- forkIO $ forever $ do
    decodeOSC <$> recv inbox 4096 >>= \case
      Left text -> putStrLn . show $ text
      Right osc -> do
        case readOSC_asSwitch osc of
          Left s -> putStrLn s
          Right (monome,switch) ->
            handleSwitch mst monome switch >>=
            either putStrLn return

  let loop :: IO (St JiApp) =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          killThread responder
          st <- readMVar mst
          let f = maybe (putStrLn "voice with no synth") free
            in mapM_ (f . (^. voiceSynth)) (M.elems $ _stVoices st)
          _ <- send toMonome $ allLedOsc Monome_256 False
          return st
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop


-- ** Utilities they use

-- | Every time serialosc is restarted, the prefix that a monome uses
-- to filter messages (i.e. to decide which ones to respond to) is reset,
-- to "monome". I don't want them to all respond to the same messages,
-- so I have to give them different prefixes initially.
-- (Alternatively I could have them listen to different ports, I think,
-- but this is easy and that might not be.)
renameMonomes :: IO ()
renameMonomes = do
  toMonome128 <- sendsTo (unpack localhost) 14336
  toMonome256 <- sendsTo (unpack localhost) 15226
  _ <- send toMonome128 $ encodeOSC $ OSC "/sys/prefix"
    [ OSC_S $ pack $ show Monome_128 ]
  _ <- send toMonome256 $ encodeOSC $ OSC "/sys/prefix"
    [ OSC_S $ pack $ show Monome_256 ]
  return ()

initAllWindows :: forall app. MVar (St app) -> IO ()
initAllWindows mst = do
  st <- readMVar mst
  let runWindowInit :: (MonomeId, Window app) -> IO ()
      runWindowInit (mi, w) = let
        st' :: St app = st & stPending_Monome %~ flip (++)
                             (windowInitLeds w st mi)
        in mapM_ (either putStrLn id) $
           doLedMessage st' <$> _stPending_Monome st'
      mws :: [(MonomeId, Window app)] =
        concatMap (\(m,ws) -> map (m,) ws) $
        M.toList $ _stWindowLayers st
  mapM_ runWindowInit mws

-- | Called every time a monome button is pressed or released.
-- Does two kinds of IO: talking to SuperCollider and changing the MVar.
-- TODO : instead of MVar IO, return an St -> St.
-- (That way I could test it.)
handleSwitch :: forall app.
                MVar (St app) -> MonomeId -> ((X,Y), Switch)
             -> IO (Either String ())
-- PITFALL: The order of execution in `handleSwitch` is complex.
-- `windowHandler` runs before `doScAction` and `doLedMessage`.
-- Thus some updates to the St (everything but voice parameters,
-- the `voiceSynth` field, and voice deletion)
-- happen before SC has been informed.

handleSwitch mst mi sw@ (btn,_) = do
  st0 <- takeMVar mst
  let
    go :: [Window app] -> IO (Either String ())
    go [] = return $ Left $
     "Switch " ++ show sw ++ " claimed by no Window."
    go (w:ws) = case windowContains w btn of
      False -> go ws
      True -> case windowHandler w st0 (mi,sw) of
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

  case M.lookup mi $ _stWindowLayers st0 of
    Nothing -> return $ Left $ "WIndows for " ++ show mi ++ " not found."
    Just ws ->
      fmap (mapLeft ("Monome.Main.handleSwitch: " ++)) $
        go ws

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
      s :: Synth MoopParams <-
        maybe (Left $ "VoiceId " ++ show vid ++ " has no assigned synth.")
        Right $ (_stVoices st M.! vid) ^. voiceSynth
      let go :: (ParamName, Float) -> Either String (IO ())
          go (param, f) =
             case param of
               "amp"  -> Right $ set s (toI f :: I "amp")
               "freq" -> Right $ set s (toI f :: I "freq")
               "lag"  -> Right $ set s (toI f :: I "lag")
               _      -> Left $ "unrecognized parameter " ++ param
      ios :: [IO ()] <- mapM go $ M.toList $ _actionScMsg sca
      Right $ mapM_ id ios >> return (Right id)

    ScAction_New _ _ _ -> do
      let vid :: VoiceId = _actionSynthName sca
      _ <- maybe (Left $ "VoiceId " ++ show vid ++ " should be present "
                  ++ " (but not yet with a synth) in _stVoices.")
           Right $ M.lookup vid $ _stVoices st
      Right $ do
        s <- synth moop () -- TODO change moop to `_actionSynthDefEnum sca`
        let go :: (ParamName, Float) -> Either String (IO ())
            go (param, f) =
               case param of
                 "amp"  -> Right $ set s (toI f :: I "amp")
                 "freq" -> Right $ set s (toI f :: I "freq")
                 "lag"  -> Right $ set s (toI f :: I "lag")
                 _      -> Left $ "unrecognized parameter " ++ param
        case mapM go $ M.toList $ _actionScMsg sca of
          Left err -> return $ Left err
          Right (ios :: [IO ()]) -> do
            mapM_ id ios
            return $ Right $
              stVoices . at vid . _Just . voiceSynth .~ Just s

    -- PITFALL: If a voice is deleted right away,
    -- there's usually an audible pop.
    -- (It depends on how far the waveform is displaced from 0.)
    -- To avoid that, this first sends an `amp=0` message,
    -- and then waits a bit (how much is determined in Monome.Config).
    -- (That smooths the click because amp messages are responded to a tad
    -- sluggishly, per the following line in the synth's definition:
    -- s1 <- lag (in_ (V::V "amp"), lagSecs_ 0.03)
    ScAction_Free _ _ -> do
      let vid :: VoiceId = _actionSynthName sca
      v :: Voice a <- maybe
        (Left $ "VoiceId " ++ show vid ++ " absent from _stVoices.")
        Right $ M.lookup vid $ _stVoices st
      s :: Synth MoopParams <- maybe
        (Left $ "Voice " ++ show vid ++ " has no assigned synth.")
        Right $ _voiceSynth v
      Right $ do
        now <- unTimestamp <$> getTime
        set s (0 :: I "amp")
        doScheduledAt ( Timestamp $ fromRational now
                        + realToFrac Config.freeDelay ) $
          free s
        return $ Right $ stVoices . at vid .~ Nothing

doLedMessage :: St app -> LedMsg -> Either String (IO ())
doLedMessage st ((mi, wi), (xy, b)) =
  mapLeft ("doLedMessage: " ++) $
  case relayToWindow st mi wi of
    Left s         -> Left s
    Right toWindow -> Right $ toWindow (xy,b)
