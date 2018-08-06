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

dunno = do
  dist <- newDistrib
  act (reg dist) $ snd $ (V.!) (_vec msq) 0

-- | call like this:
-- tid <- handTest
handTest = do
  dist <- newDistrib
  swapMVar (mTimeMuseqs dist) $ M.fromList [("1",(0::Time, msq))]
  startDistribLoop dist
