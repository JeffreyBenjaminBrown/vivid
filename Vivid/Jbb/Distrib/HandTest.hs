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
                                , (s+n, Send Boop "marge" ("freq",333))
                                , (s+n, Send Boop "marge" ("amp",0.2))
                                , (0.99, Free Boop "marge")] }

msq' on off = Museq { _dur = 1,
                   _vec = V.fromList [ (on, Send Boop "marge" ("freq",333))
                                     , (on, Send Boop "marge" ("amp",0.2))
                                     , (off, Send Boop "marge" ("amp",0)) ] }

actIsWorking = do
  dist <- newDistrib
  act (reg dist) $ New Boop "fred"
  putStrLn =<< show <$> readMVar ( boops $ reg $ dist)
    -- `act` called on a `New` indeed creates that synth

  act (reg dist) $ Send Boop "fred" ("freq", 444)
  act (reg dist) $ Send Boop "fred" ("amp", 0.4)

  wait 1
  act (reg dist) $ Free Boop "fred"

-- tid <- oneLoop $ msq 0 0.2 -- or some such parameters
-- tid <- oneLoop $ msq 0.2 0.6 -- or some such parameters
-- killThread tid
oneLoop museq = do
  dist <- newDistrib
  swapMVar (mTimeMuseqs dist) $ M.fromList [("1",(0,museq))]
  startDistribLoop dist

m1 = Museq {_dur = 1, _vec = V.fromList
             [ (0,   Send Boop "1" ("freq",400) )
             , (0,   Send Boop "1" ("amp",0.4)  )
             , (0.5, Send Boop "1" ("amp",0)    ) ] }

m2 = Museq {_dur = 2, _vec = V.fromList
             [ (0,   Send Boop "2" ("amp",0)    )
             , (0.5, Send Boop "2" ("freq",500) )
             , (0.5, Send Boop "2" ("amp",0.4)  ) ] }

-- TODO BUG : m2' should be silent sometimes
-- It is never silent as long as it starts after time 0,
-- and yet `oneLoop $ msq 0.2 0.6` works fine.
-- Moreover if I change start from 0.01 to 0.2 I hear no difference.
-- UNLESS it's the only voice. Then it makes a difference -- but it can
-- take a few cycles to respond to a swapMVar.
m2' start = Museq {_dur = 2, _vec = V.fromList
             [ (start, Send Boop "2" ("amp",0)    )
             , (0.5,   Send Boop "2" ("freq",550) )
             , (0.5,   Send Boop "2" ("amp",0.4)  )
             , (0.75,  Send Boop "2" ("freq",650) )
             , (0.75,  Send Boop "2" ("amp",0.4)  ) ] }

-- | like m2' but with a dummy action at time 0. Still doesn't work.
m2'' start = Museq {_dur = 2, _vec = V.fromList
             [ (0,     Send Boop "2" ("freq",550)    )
             , (start, Send Boop "2" ("amp",0)    )
             , (0.5,   Send Boop "2" ("freq",550) )
             , (0.5,   Send Boop "2" ("amp",0.4)  )
             , (0.75,  Send Boop "2" ("freq",650) )
             , (0.75,  Send Boop "2" ("amp",0.4)  ) ] }

m3 = Museq {_dur = 2, _vec = V.fromList
             [ (0,   Send Boop "3" ("amp",0)    )
             , (0.5, Send Boop "3" ("freq",650) )
             , (0.5, Send Boop "3" ("amp",0.4)  ) ] }

-- (tid, dist) <- twoLoops
-- m <- readMVar $ mTimeMuseqs dist
-- swapMVar (mTimeMuseqs dist) $ M.insert "2" (0,m2' 0.2) m
-- killThread tid
twoLoops :: IO (ThreadId, Distrib)
twoLoops = do
  dist <- newDistrib
  swapMVar (mTimeMuseqs dist) $ M.fromList [ ("1",(0,m1)),
                                             ("2",(0,m2)) ]
  mapM_ (act $ reg dist) $ concatMap newsFromMuseq [m1,m2,m2' 0,m3,msq' 0 0.5]
  tid <- startDistribLoop dist
  return $ (tid, dist)
