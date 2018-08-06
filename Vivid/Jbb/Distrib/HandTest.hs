module Vivid.Jbb.Distrib.HandTest where

import           Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Types


msq = Museq {_dur = 2,
              _vec = V.fromList [(0, New Boop "marge")
                                ,(0.1, Send Boop "marge" ("freq",444))
                                ,(0.1, Send Boop "marge" ("amp",0.2))
                                ,(0.5, Free Boop "marge")] }

actIsWorking = do
  dist <- newDistrib
  act (reg dist) $ New Boop "fred"
  putStrLn =<< show <$> readMVar ( boops $ reg $ dist)
    -- `act` called on a `New` indeed creates that synth

  act (reg dist) $ Send Boop "fred" ("freq", 444)
  act (reg dist) $ Send Boop "fred" ("amp", 0.1)

  wait 1
  act (reg dist) $ Free Boop "fred"

-- | Call it, and then end it, like this:
-- tid <- loopTest
-- killThread tid
loopTest = do
  dist <- newDistrib
  swapMVar (mTimeMuseqs dist) $ M.fromList [("1",(0::Time, msq))]
  startDistribLoop dist
