{-# LANGUAGE
LambdaCase
, OverloadedStrings
, ScopedTypeVariables
#-}

module Montevideo.Monome.Network.ListenAndLogOsc where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)

import qualified Network.Socket as NS
import Vivid.OSC

import Montevideo.Monome.Network.Util


-- ^ Tries to read as OSC, then prints (as OSC or otherwise).
-- Also accumulates a list of OSC messages.
-- Useful when running `requestDeviceList` or `requestDeviceInfo`
-- from another repl.
listenAndLogOsc
  :: Int -- ^ the port to listen to
  -> IO [OSC]
listenAndLogOsc port = do

  skt :: NS.Socket <- receivesAt "127.0.0.1" port
  acc <- newMVar []
  let loop :: IO [OSC]
      loop = getChar >>=
        \case 'q' -> close skt >> readMVar acc >>= return
              _   -> loop
      logAndShow :: OSC -> IO ()
      logAndShow osc = do accNow <- takeMVar acc
                          putMVar acc $ osc : accNow
                          putStrLn . show $ osc
      logAndShowEitherOsc :: Either String OSC -> IO ()
      logAndShowEitherOsc (Left s) = putStrLn $ show s
      logAndShowEitherOsc (Right osc) = logAndShow osc
  _ <- forkIO $ forever $
       decodeOSC <$> recv skt 4096 >>= logAndShowEitherOsc
  loop
