module Vivid.Jbb.Distrib.HandTest where

import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Act
import Vivid.Jbb.Distrib.Distrib
import Vivid.Jbb.Distrib.Types


-- | Since a SynthDef takes time to be instantiated,
-- a sequence like this doesn't work if `n` is small enough.
msq s n = Museq { _dur = 1,
              _vec = V.fromList [ (s, New Boop "marge")
                                , (s+n, Send Boop "marge" ("freq",444))
                                , (s+n, Send Boop "marge" ("amp",0.2))
                                , (0.99, Free Boop "marge")] }

actIsWorking = do
  dist <- newDistrib
  act (reg dist) $ New Boop "fred"
  putStrLn =<< show <$> readMVar ( boops $ reg $ dist)
    -- `act` called on a `New` indeed creates that synth

  act (reg dist) $ Send Boop "fred" ("freq", 444)
  act (reg dist) $ Send Boop "fred" ("amp", 0.1)

  wait 1
  act (reg dist) $ Free Boop "fred"

-- tid <- oneLoop $ msq 0 0.2 -- or some such parameters
-- killThread tid
oneLoop msq = do
  dist <- newDistrib
  swapMVar (mTimeMuseqs dist) $ M.fromList [("1",(0,msq))]
  startDistribLoop dist

-- tid <- twoLoops
-- killThread tid
twoLoops :: IO ThreadId
twoLoops = do
  dist <- newDistrib
  let m1 = Museq {_dur = 1, _vec = V.fromList
                                   [ (0,   Send Boop "1" ("freq",444) )
                                   , (0,   Send Boop "1" ("amp",0.1)  )
                                   , (0.5, Send Boop "1" ("amp",0)    ) ] }
      m2 = Museq {_dur = 2, _vec = V.fromList
                                   [ (0,   Send Boop "2" ("amp",0)    )
                                   , (0.5, Send Boop "2" ("freq",555) )
                                   , (0.5, Send Boop "2" ("amp",0.1)  ) ] }
  swapMVar (mTimeMuseqs dist) $ M.fromList [ ("1",(0,m1))
                                           , ("2",(0,m2)) ]
  mapM_ (act $ reg dist) $ concatMap newsFromMuseq [m1,m2]
  startDistribLoop dist
