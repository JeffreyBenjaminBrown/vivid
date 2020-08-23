-- * This is for configuring the monome itself --
-- reading and writing the port that it uses, the prefix it filters by, etc.
-- I don't seem to use any of the definitions here,
-- just the commented-out code that precedes them.

{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Config.Monome where

import qualified Network.Socket.ByteString as NSB
import Vivid.OSC

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types.Device


-- | Test the monome.

-- | This documentation for serialosc is critical:
--   https://monome.org/docs/serialosc/osc/
-- How to interpret it: Something like `/serialosc/list si <host> <port>`
-- means `/serialosc/list` takes two arguments, a string and an int,
-- the first being the host and the second being the port.
--
-- This documentation is worse and hopefully unnecessary:
--   https://monome.org/docs/serialosc/serial.txt

-- | Before any of the following, run this in a separate GHCI instance,
-- to read output from the commands below.
-- (I don't know where the magic number 8000 comes from.)
--   listenAndLogOsc 8000

-- | To find all connected monomes. In particular,
-- the last item in each response indicates which port that monome listens to,
-- which is what makes creating the `sendsTo` instances below possible.
--   toSerialosc <- sendsTo (Char8.unpack localhost) 12002
--   send toSerialosc $ requestDeviceList 8000

-- | To find more information about a monome, do the following.
-- (It requires knowing which port number a monome listens to;
-- run `requestDeviceList` as above to get that.
--   toMonome128 <- sendsTo (Char8.unpack localhost) 14336
--   toMonome256 <- sendsTo (Char8.unpack localhost) 15226
--   send toMonome128 $ requestDeviceInfo 8000
--   send toMonome256 $ requestDeviceInfo 8000

-- | To change the prefix a monome filters by:
--   send toMonome128 $ encodeOSC $ OSC "/sys/prefix" [ OSC_S "128" ]
--   send toMonome256 $ encodeOSC $ OSC "/sys/prefix" [ OSC_S "256" ]

-- | To test a monome's LEDs:
-- send toMonome $ fade "/monome" 0 1 15 -- 15 is brightness
  -- lower nonzero brightness values are like 0 on one of the monomes
-- send toMonome $ ledOsc "/monome" ((6,6) , True)
  

-- | = Send a message to something

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
