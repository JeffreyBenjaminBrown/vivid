{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, FlexibleContexts
, LambdaCase
, OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Montevideo.Monome.Main (
    edoMonome
  , jiMonome
  ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Lens
import           Data.ByteString.Char8 (unpack)
import qualified Data.Map as M
import Vivid
import Vivid.OSC

import qualified Montevideo.Monome.Config as Config
import Montevideo.Monome.Network.Util
import Montevideo.Synth
import Montevideo.Monome.Util.Button
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Window.Util
import Montevideo.Monome.Window.JI
import Montevideo.Monome.Window.Keyboard
import Montevideo.Monome.Window.Shift
import Montevideo.Monome.Window.Sustain


edoMonome :: Int -- ^ The monome address, as serialoscd reports on startup.
          -> IO (St EdoApp)
edoMonome monomePort = do
  inbox :: Socket <- receivesAt "127.0.0.1" 8000
    -- I don't know why it's port 8000, or why it used to be 11111.
  toMonome :: Socket <- sendsTo (unpack localhost) monomePort
    -- to find the port number above, use the first part of HandTest.hs

  mst <- newMVar $ St {
      _stWindowLayers = [sustainWindow, shiftWindow, keyboardWindow]
    , _stToMonome = toMonome
    , _stVoices = mempty
    , _stPending_Vivid = []
    , _stPending_Monome = []

    , _stApp = EdoApp
        { _edoConfig = Config.theConfig
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
          Right switch ->
            handleSwitch mst switch >>=
            either putStrLn return

  let loop :: IO (St EdoApp) =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          killThread responder
          st <- readMVar mst
          let f = maybe (putStrLn "voice with no synth") free
            in mapM_ (f . (^. voiceSynth)) (M.elems $ _stVoices st)
          _ <- send toMonome $ allLedOsc "/monome" False
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

  inbox :: Socket <- receivesAt "127.0.0.1" 8000
  toMonome :: Socket <- sendsTo (unpack localhost) monomePort

  mst <- newMVar $ St {
      _stWindowLayers = [jiWindow]
    , _stToMonome = toMonome
    , _stVoices = mempty
    , _stPending_Vivid = []
    , _stPending_Monome = []
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
          Right switch ->
            handleSwitch mst switch >>=
            either putStrLn return

  let loop :: IO (St JiApp) =
        getChar >>= \case
        'q' -> do -- quit
          close inbox
          killThread responder
          st <- readMVar mst
          let f = maybe (putStrLn "voice with no synth") free
            in mapM_ (f . (^. voiceSynth)) (M.elems $ _stVoices st)
          _ <- send toMonome $ allLedOsc "/monome" False
          return st
        _   -> loop
    in putStrLn "press 'q' to quit"
       >> loop
