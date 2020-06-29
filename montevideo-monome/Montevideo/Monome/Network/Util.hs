{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Network.Util (
    close          -- ^ Socket -> IO ()
  , recv           -- ^ Socket -> Int -> IO ByteString
  , send           -- ^ Socket -> ByteString -> IO Int
  , HostName       -- ^ HostName
  , Socket         -- ^ Socket
  , localhost      -- ^ ByteString
  , toPort         -- ^ Show a => a -> IO Socket
  , toSerialosc    -- ^ IO Socket
  , getLocalSocket -- ^ Show a => HostName -> a -> IO (Socket, NS.AddrInfo)
  , sendsTo        -- ^ Show a => HostName -> a -> IO Socket
  , receivesAt     -- ^ Show a => HostName -> a -> IO Socket
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Montevideo.Monome.Types.Initial (HostName, Socket)


-- | = PITFALL: Ports 0-1024 are reserved.
-- All 5-digit numbers seem to work, though.

close :: Socket -> IO ()
close = NS.close

recv :: Socket -> Int -> IO ByteString
recv = NSB.recv

send :: Socket -> ByteString -> IO Int
send = NSB.send

localhost :: ByteString
localhost = "127.0.0.1"

toPort :: Show a => a -> IO Socket
toPort port = sendsTo (unpack localhost) port
toSerialosc :: IO Socket
toSerialosc = toPort 12002
  -- ^ https://monome.org/docs/serialosc/osc/

getLocalSocket :: Show a => HostName -> a -> IO (Socket, NS.AddrInfo)
getLocalSocket host port = do
  (a:_) <- NS.getAddrInfo Nothing (Just host) (Just $ show port)
  s <- NS.socket (NS.addrFamily a) NS.Datagram NS.defaultProtocol
  return (s,a)

sendsTo :: Show a => HostName -> a -> IO Socket
sendsTo host port = do
  (s,a) <- getLocalSocket host port
  NS.connect s $ NS.addrAddress a
  return s

receivesAt :: Show a => HostName -> a -> IO Socket
receivesAt host port = do
  (s,a) <- getLocalSocket host port
  NS.bind s $ NS.addrAddress a
  return s
