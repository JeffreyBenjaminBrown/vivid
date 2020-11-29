module Montevideo.Dispatch.Config where

import Montevideo.Dispatch.Types


-- | Arcs are computed one frame at a time.
-- Frame duration also determines how long it takes to delete a synth:
-- half a frame after a synth is silenced, it is deleted
-- (by dispatchConsumeScAction_Free).
frameDuration :: Time
frameDuration = 0.1

-- | 3 ms after sending a trigger=1 message to a sampler,
-- send a trigger=0 message.
-- This is necessary because the samplers are trigger when they detect
-- a change from negative to positive.
retriggerLag :: Time
retriggerLag = 0.003
