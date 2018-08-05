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



findNextEvents :: Time -> Duration -> Time
               -> Museq -> (Duration, [Action])
findNextEvents time0 globalPeriod now museq =

--  last phase 0 was at 21, so now is just before halfway through
bp s = New Boop s
events = [(0,bp "a"), (0.25,bp"b"), (0.5,bp"c"), (0.5,bp"d"), (0.75,bp"e")]
-- findNextEvents time0 globalPeriod now museq
findNextEvents 1 10 30 $ Museq 2 $ V.fromList events
-- (pp0, elapsed, relNow, vecLen, start, end)


compare' ve ve' = compare (fst ve) (fst ve')
dummyAction = New Boop "marge"
firstIndexGTE compare' (V.fromList events) 

firstIndexMoreThanGTE compare (V.fromList [1..5]) 3
