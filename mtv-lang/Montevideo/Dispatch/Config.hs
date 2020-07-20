module Montevideo.Dispatch.Config where

import Montevideo.Dispatch.Types


-- | Half a frame after a synth is silenced, it is deleted.
frameDuration :: Time
frameDuration = 0.1

-- | 3 ms after sending a trigger=1 message to a sampler,
-- send a trigger=0 message.
-- This is necessary because the samplers are trigger when they detect
-- a change from 0 to 1.
retriggerLag :: Time
retriggerLag = 0.003
