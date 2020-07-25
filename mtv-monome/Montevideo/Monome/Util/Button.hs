{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Util.Button (
    X, Y, Switch, Led, LedBecause(..)
  , readOSC_asSwitch, fromBool, boolFromInt
  , ledOsc, allLedOsc
  ) where

import Data.Either.Combinators
import Vivid.OSC

import Montevideo.Monome.Network.Monome
import Montevideo.Monome.Types.Initial
import Montevideo.Util
import Data.ByteString (ByteString)


fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

boolFromInt :: Int -> Either String Bool
boolFromInt 0 = Right False
boolFromInt 1 = Right True
boolFromInt x = Left ( "boolFromInt: " ++ show x
                       ++ " is niether 0 nor 1." )

readOSC_asSwitch :: OSC -> Either String ((X,Y), Switch)
readOSC_asSwitch m =
  mapLeft ("readOSC_asSwitch" ++) $
  case m of
    (OSC "/monome/grid/key" [OSC_I x, OSC_I y, OSC_I s]) -> do
      b <- boolFromInt $ fi s
      Right ((fi x, fi y), b)
    x ->
      Left $ "readOSC_asSwitch: Bad OSC message: " ++ show x

-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ fromBool led

-- | Tells the monome to turn on every LED. See Test/HandTest.hs.
allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ fromBool led
