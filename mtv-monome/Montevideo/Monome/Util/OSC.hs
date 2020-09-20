{-# LANGUAGE OverloadedStrings
, ViewPatterns
#-}

module Montevideo.Monome.Util.OSC (
  -- * re-exports
    X, Y, Switch, Led, LedBecause(..)

  -- * This isn't really OSC-specific but it's pretty close.
  , allMonomeIds -- ^ [MonomeId]

  -- * OSC
  , readOSC_asSwitch -- ^ OSC -> Either String ( MonomeId, ((X,Y), Switch))
  , ledOsc           -- ^ MonomeId -> ((X,Y), Led) -> ByteString  
  , allLedOsc        -- ^ MonomeId -> Led -> ByteString

  -- * Monome-related conversions for `String`, `Int`, `Bool`, `MonomeId`
  , inverseShowMonome -- ^ String -> Either String MonomeId
  , fromBool          -- ^ Num a => Bool -> a
  , boolFromInt       -- ^ Int -> Either String Bool
  ) where

import Data.ByteString.Char8
import Data.Either.Combinators
import qualified Data.List as L
import Vivid.OSC

import Montevideo.Monome.Network.Monome
import Montevideo.Monome.Types.Most
import Montevideo.Util


allMonomeIds :: [MonomeId]
allMonomeIds = [ Monome_128
               , Monome_256
               , Monome_old ]

-- * OSC

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

-- | Tells the monome to turn on an LED. See Test/HandTest.hs.
ledOsc :: MonomeId -> ((X,Y), Led) -> ByteString
ledOsc prefix ((x, y), led) =
  onoff ('/' : show prefix) x y $ fromBool led

-- | Tells the monome to light or darken *every* LED. See Test/HandTest.hs.
allLedOsc :: MonomeId -> Led -> ByteString
allLedOsc prefix led =
  allLeds ('/' : show prefix) $ fromBool led


-- * Monome-related conversions for `String`, `Int`, `Bool`, `MonomeId`

inverseShowMonome :: String -> Either String MonomeId
inverseShowMonome s =
  mapLeft ("inverseShowMonome: " ++) $
  maybe (Left $ "MonomeId for " ++ s ++ " not found.")
  Right $ L.find ((==) s . show) allMonomeIds

fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

boolFromInt :: Int -> Either String Bool
boolFromInt 0 = Right False
boolFromInt 1 = Right True
boolFromInt x = Left ( "boolFromInt: " ++ show x
                       ++ " is niether 0 nor 1." )
