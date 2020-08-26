{-# LANGUAGE OverloadedStrings
, ViewPatterns
#-}

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
readOSC_asSwitch :: OSC -> Either String ( MonomeId
                                         , ((X,Y), Switch))
readOSC_asSwitch m@(OSC str l) =
  mapLeft ("readOSC_asSwitch" ++) $
  let ms :: [String] = lines' '/' $ unpack str
      err = Left $ "Unrecognized OSC message: " ++ show m
  in case l of
       [OSC_I x, OSC_I y, OSC_I s] -> do
         b <- boolFromInt $ fi s
         case ms of
           [t1,t2,t3] ->
             if t2 == "grid" && t3 == "key"
             then let press = ((fi x, fi y), b)
                  in case inverseShowMonome t1 of
                       Right monome -> Right (monome, press)
                       Left _ -> err
             else err
           _ -> err
       _ -> err

inverseShowMonome :: String -> Either String MonomeId
inverseShowMonome ((==) (show Monome_256) -> True) = Right Monome_256
inverseShowMonome ((==) (show Monome_128) -> True) = Right Monome_128
inverseShowMonome m = Left $ "Monome not found: " ++ m ++ "."

-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: MonomeId -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) =
  onoff ('/' : show prefix) x y $ fromBool led

-- | Tells the monome to light or darken *every* LED. See Test/HandTest.hs.
allLedOsc :: MonomeId -> Led -> ByteString
allLedOsc prefix led =
  allLeds ('/' : show prefix) $ fromBool led
