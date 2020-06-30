{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Types.Button (
    X, Y, Switch, Led, LedBecause(..)
  , readSwitchOSC, fromBool, boolFromInt
  , ledOsc, allLedOsc
  ) where

import Vivid.OSC

import Montevideo.Monome.Network.Monome
import Montevideo.Monome.Types.Initial
import Montevideo.Util
import Data.ByteString (ByteString)


fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

boolFromInt :: Int -> Bool
boolFromInt 0 = False
boolFromInt 1 = True
boolFromInt x = error $ "boolFromInt: " ++ show x
                  ++ " is niether 0 nor 1."

readSwitchOSC :: OSC -> ((X,Y), Switch)
readSwitchOSC (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) =
  ((fi x, fi y), boolFromInt $ fi s)
readSwitchOSC x = error $ "readSwitchOSC: bad message: " ++ show x

-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ fromBool led

-- | Tells the monome to turn on every LED. See Test/HandTest.hs.
allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ fromBool led
