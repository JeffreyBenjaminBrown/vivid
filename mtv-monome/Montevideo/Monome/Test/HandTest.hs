{-# LANGUAGE OverloadedStrings
, TupleSections #-}

module Montevideo.Monome.Test.HandTest where

import qualified Network.Socket.ByteString as NSB
import Vivid.OSC

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types.Device


-- | = Send a message to something

testToPort :: Show a => a -> IO Int
testToPort port = do
  s <- toPort port
  NSB.send s $ encodeOSC $ OSC "/testing" [ OSC_S "testing" ]


-- | Test the monome.

-- listenAndLogOsc 8000
-- toSerialosc <- sendsTo (unpack localhost) 12002
-- send toSerialosc $ requestDeviceList 8000

-- To get the right port number for toMonome, run the previous two lines.
-- toMonome <- sendsTo (unpack localhost) 14718
-- send toMonome $ requestDeviceInfo 8000

-- send toMonome $ fade "/monome" 0 1 15 -- 15 is brightness
  -- lower nonzero brightness values are like 0 on one of the monomes
-- send toMonome $ ledOsc "/monome" ((6,6) , True)

-- mapM (send toMonome . ledOsc "/monome" . (,True)) $ enharmonicToXYs (0,15)
  
d :: Device
d = readDevice [
  OSC "/sys/id" [OSC_S "m0000102"]
  , OSC "/sys/size" [OSC_I 16,OSC_I 16]
  , OSC "/sys/host" [OSC_S "127.0.0.1"]
  , OSC "/sys/port" [OSC_I 8000]
  , OSC "/sys/prefix" [OSC_S "/monome"]
  , OSC "/sys/rotation" [OSC_I 0]
  ]

-- readOSC_asSwitch $ OSC "/monome/grid/key" [OSC_I 7, OSC_I 7, OSC_I 1]
