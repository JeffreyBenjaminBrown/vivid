{-# LANGUAGE TupleSections #-}

module Vivid.Jbb.Dispatch.Dispatch_2 where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Random
import System.Random.MWC

import Vivid
import Vivid.Jbb.Dispatch.ActNow
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types

import Vivid.Jbb.Dispatch.Dispatch -- temp


startDispatchLoop_2 :: Dispatch -> IO ThreadId
startDispatchLoop_2 dist = do
  tryTakeMVar $ mTime0 dist -- empty it, just in case
  (+(-0.05)) . unTimestamp <$> getTime >>= putMVar (mTime0 dist)
    -- add .05 so music starts in .05 seconds, not frameDur seconds
  forkIO $ dispatchLoop_2 dist

dispatchLoop_2 _ = return ()
