{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Config.Monome where

import qualified Network.Socket.ByteString as NSB
import Vivid.OSC

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types


type Port = Int

monomePort :: MonomeId -> Port
monomePort Monome_256 = 15226
monomePort Monome_128 = 14336


-- | * Send a message to something.
-- PITFALL: Unused.

testToPort :: Show a => a -> IO Int
testToPort port = do
  s <- toPort port
  NSB.send s $ encodeOSC $ OSC "/testing" [ OSC_S "testing" ]

device :: Device
device = readDevice [
  OSC "/sys/id" [OSC_S "m0000102"]
  , OSC "/sys/size" [OSC_I 16,OSC_I 16]
  , OSC "/sys/host" [OSC_S "127.0.0.1"]
  , OSC "/sys/port" [OSC_I 8000]
  , OSC "/sys/prefix" [OSC_S "/monome"]
  , OSC "/sys/rotation" [OSC_I 0]
  ]
