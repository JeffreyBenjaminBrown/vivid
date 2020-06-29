{-# LANGUAGE DeriveGeneric #-}

module Montevideo.Dispatch.Test.Scheduling_and_CSV where

import Data.ByteString.Lazy.Char8 (unpack)
import Control.Concurrent.MVar
import Data.Csv
import Data.List.Split (splitOn)
import GHC.Generics (Generic)

import Vivid
import Util
import Dispatch.Config
import Dispatch.Types
import Dispatch.Dispatch
import Dispatch.Museq


-- | = arc is messed up

--m = museq 5 [((0,6),"a"),((2,4),"b")]
--
--arcTest1 = arcIO 0   0.99 0   10  m
--arcTest2 = arcIO 100 0.99 100 110 m
--arcTest3 = arcIO 0   1.01 0   10  m
--arcTest4 = arcIO 100 1.01 100 110 m -- TODO BUG
--  -- The bug: Again, floating point error:
--  -- pp0 is sometimes off by one.

testChTempoPeriod :: Dispatch -> Duration -> IO Frame
testChTempoPeriod disp newTempoPeriod = do
  time0       <- readMVar $ mTime0       disp
  tempoPeriod <- readMVar $ mTempoPeriod disp
  now         <- unTimestamp <$> getTime
  let np0 = nextPhase0 time0 frameDuration now
      startRender = np0 + 2 * frameDuration
      startRenderInCycles = (startRender - time0) / tempoPeriod
      newTime0 = startRender - startRenderInCycles * newTempoPeriod
  return $ Frame { frameTempoPeriod = fromRational $ tempoPeriod
                 , frameNow = fromRational $ toRational now - time0
                 , frameNp0 = fromRational $ np0 - time0
                 , frameStartRender = fromRational $ startRender - time0
                 , frameStartRenderInCycles =
                   fromRational $ startRenderInCycles
                 , frameNewTime0 = fromRational $ newTime0 - time0
                 }

data Frame = Frame { frameTempoPeriod :: Double
                   , frameNow :: Double
                   , frameNp0 :: Double
                   , frameStartRender :: Double
                   , frameStartRenderInCycles :: Double
                   , frameNewTime0 :: Double} deriving (Show, Generic)

-- Generics magic
instance FromNamedRecord Frame
instance ToNamedRecord Frame
instance DefaultOrdered Frame

encodeExample = splitOn "\r\n" $ unpack
  $ encodeDefaultOrderedByName [Frame 1 1 1 1 1 1]

