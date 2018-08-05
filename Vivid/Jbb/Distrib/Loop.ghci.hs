import Control.Concurrent.MVar

-- | pseudocode for Distrib.Loop.loop
-- setup
--   time0 <- now + .01 -- latency is around 3 ms, so this 10 ms lead is safe
-- loop
--   now <- getNow
--   let nextEvents_timeUntil = ...
--       nextEvents = ...
--   wait nextEvents_timeUntil
--   do nextEvents
--   loop

reg <- emptySynthRegister
mPeriod <- newMVar 1
