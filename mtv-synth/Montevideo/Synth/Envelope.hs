{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Montevideo.Synth.Envelope where

import Vivid


-- | PITFALL: "on" should only take on the values 0 or 1.
-- | PITFALL: "on" is a little misleading, as when it switches from 1 to 0,
-- the envelope does not immediately end; rather, it begins to ramp down to 0.
-- When "on" becommes 1, the envelope swells from 0 to 1.
-- When it becomes 0, the envelope falls from 1 to 0.
-- The signals "att" and "rel" give the attack and release times.
--
-- An ADSR envelope would be more natural (and flexible) than the
-- following weird patch of two envelopes, but I couldn't figure ADSR out.

onOffEnvelope :: (Elem "on" a, Elem "att" a, Elem "rel" a)
              => SDBody' a Signal
onOffEnvelope = let
  onEnv = envGen_wGate -- The "on (swell) envelope".
          (V::V "on")
          (V::V "att")
          ( env 0 -- This initial 0 is not revisited on retriggering
                  -- (which happens each time the gate becomes positive).
            [ (0,0) -- Hence this silly-looking first pair,
                    -- which says "go to level 0 in 0 seconds".
            , (1,1) ]
            Curve_Lin )
          DoNothing
  offEnv = -- The "off (decay) envelope".
    biOp Max (V::V "on") -- Must take the Max of "on" and the envelope below,
                         -- because once the off envelope falls to 0,
                         -- it stays there until it is triggered again.
                         -- In particular it is 0 even when onEnv triggers.
    ( envGen_wGate -- The "off envelope".
      (1 ~- (V::V "on"))
      (V::V "rel")
      (env 1 -- This initial 1 is not revisited on retriggering
        -- (which happens each time the gate becomes positive).
        [ (1,0) -- Hence this silly-looking first pair,
          -- which says "go to level 1 in 0 seconds".
        , (0,1) ]
        Curve_Lin)
      DoNothing )
  in onEnv ~* offEnv
