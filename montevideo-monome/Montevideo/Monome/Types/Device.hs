{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Types.Device (
  DeviceID(..), readDeviceID,
  Device(..), readDevice
  ) where

import Vivid.OSC

import Montevideo.Monome.Network.Util
import Montevideo.Monome.Types.Button
import Montevideo.Util
import Montevideo.Monome.Util


-- | SerialOsc responds to /serialosc/list messages with this information.
data DeviceID = DeviceID { deviceIDName :: ByteString
                         , deviceIDType :: ByteString
                         , deviceIDPort :: Int }
  deriving (Show, Eq, Ord)

-- | A monome (distinct form serialosc!) responds to /sys/info messages
-- with this information.
--
-- A device's DeviceInfo.deviceName = its DeviceID.deviceIDName.
-- That is the only pooint where `Device` and `DeviceID` overlap.
data Device = Device {
    deviceName :: String
  , deviceSize :: (X,Y)
  , deviceHost :: HostName   -- ^ Where it sends to.
  , devicePort :: Int        -- ^ Where it sends to.
  , devicePrefix :: String   -- ^ PITFALL: Includes a leading slash.
  , deviceRotation :: Int    -- ^ 0, 90, 180 or 270
  } deriving (Show, Eq, Ord)

readDeviceID :: OSC -> DeviceID
readDeviceID ( OSC "/serialosc/device" [ OSC_S name
                                       , OSC_S monomeType
                                       , OSC_I port ] )
  = DeviceID { deviceIDName = name
             , deviceIDType = monomeType
             , deviceIDPort = fromIntegral port }
readDeviceID x = error $ "readDeviceID: unexpected message: " ++ show x

-- | PITFALL: If serialosc changed the order of its outputs, this would fail.
readDevice :: [OSC] -> Device
readDevice [a,b,c,d,e,f] = Device {
    deviceName = readDeviceName a
  , deviceSize = readDeviceSize b
  , deviceHost = readDeviceHost c
  , devicePort = readDevicePort d
  , devicePrefix = readDevicePrefix e
  , deviceRotation = readDeviceRotation f
  }
  where
    readDeviceName :: OSC -> String
    readDeviceName (OSC "/sys/id" [OSC_S name]) = unpack name
    readDeviceName x = error $ "readDeviceName: can't interpret " ++ show x

    readDeviceSize :: OSC -> (X,Y)
    readDeviceSize (OSC "/sys/size" [OSC_I x, OSC_I y]) = (fi x, fi y)
    readDeviceSize x = error $ "readDeviceSize: can't interpret " ++ show x

    readDeviceHost :: OSC -> HostName
    readDeviceHost (OSC "/sys/host" [OSC_S name]) = unpack name
    readDeviceHost x = error $ "readDeviceHost: can't interpret " ++ show x

    readDevicePort :: OSC -> Int
    readDevicePort (OSC "/sys/port" [OSC_I port]) = fi port
    readDevicePort x = error $ "readDevicePort: can't interpret " ++ show x

    readDevicePrefix :: OSC -> String
    readDevicePrefix (OSC "/sys/prefix" [OSC_S prefix]) = unpack prefix
    readDevicePrefix x =
      error $ "readDevicePrefix: can't interpret " ++ show x

    readDeviceRotation :: OSC -> Int
    readDeviceRotation (OSC "/sys/rotation" [OSC_I rotation]) = fi rotation
    readDeviceRotation x =
      error $ "readDeviceRotation: can't interpret " ++ show x
readDevice x = error $
  "readDevice: List should have length 5, instead received: " ++ show x
