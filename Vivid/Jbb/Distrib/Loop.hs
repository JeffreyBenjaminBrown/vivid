module Vivid.Jbb.Distrib.Loop where

import Control.Concurrent.MVar

import Vivid
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Distrib.Types


loop :: SynthRegister -> MVar Duration -> MVar Museq -> IO (IO ())
loop reg mTempo mMuseq = do
  let frameDur = 1 -- music is rendered frameDur seconds at a time
  mTime0 <- (\x -> x-0.05) . unTimestamp <$> getTime >>= newMVar
    -- subtract .1 so music starts in .05 seconds, not frameDur seconds
  return $ return () -- TODO
