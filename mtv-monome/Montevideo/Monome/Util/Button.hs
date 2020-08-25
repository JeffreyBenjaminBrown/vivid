{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Util.Button (
    X, Y, Switch, Led, LedBecause(..)
  , readOSC_asSwitch, fromBool, boolFromInt
  , ledOsc, allLedOsc
  ) where

import Data.Either.Combinators
import Vivid.OSC

import Montevideo.Monome.Network.Monome
import Montevideo.Monome.Types.Most
import Montevideo.Util
import Data.ByteString.Char8


fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

boolFromInt :: Int -> Either String Bool
boolFromInt 0 = Right False
boolFromInt 1 = Right True
boolFromInt x = Left ( "boolFromInt: " ++ show x
                       ++ " is niether 0 nor 1." )

-- | Example:
-- > readOSC_asSwitch $ OSC "/monome/grid/key" [OSC_I 7, OSC_I 7, OSC_I 1]
-- Right ((7,7),True)
readOSC_asSwitch :: OSC -> Either String ( String, -- ^ Which monome.
                                           ( -- ^ Which button.
                                             (X,Y), Switch))
readOSC_asSwitch m@(OSC s l) =
  mapLeft ("readOSC_asSwitch" ++) $
  let ms :: [String] = lines' '/' $ unpack s
      err = Left $ "Unrecognized OSC message: " ++ show m
  in case l of
       [OSC_I x, OSC_I y, OSC_I s] -> do
         b <- boolFromInt $ fi s
         case ms of
           ["128","grid","key"] -> do
             Right ("128",((fi x, fi y), b))
           ["256","grid","key"] -> do
             Right ("256",((fi x, fi y), b))
           _ -> err
       _ -> err


-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: String -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) = onoff prefix x y $ fromBool led

-- | Tells the monome to turn on every LED. See Test/HandTest.hs.
allLedOsc :: String -> Led -> ByteString
allLedOsc prefix led = allLeds prefix $ fromBool led
