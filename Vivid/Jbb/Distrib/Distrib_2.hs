{-# LANGUAGE TupleSections #-}

module Vivid.Jbb.Distrib.Distrib_2 where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Random
import System.Random.MWC

import Vivid
import Vivid.Jbb.Distrib.ActNow
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Museq
import Vivid.Jbb.Distrib.Types

import Vivid.Jbb.Distrib.Distrib -- temp


startDistribLoop_2 :: Distrib -> IO ThreadId
startDistribLoop_2 dist = do
  tryTakeMVar $ mTime0 dist -- empty it, just in case
  (+(-0.05)) . unTimestamp <$> getTime >>= putMVar (mTime0 dist)
    -- add .05 so music starts in .05 seconds, not frameDur seconds
  forkIO $ distribLoop_2 dist

distribLoop_2 _ = return ()
