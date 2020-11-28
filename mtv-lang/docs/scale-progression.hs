scaleStepPattern =
  mmho 3 -- Make a Museq ("mm") of duration 3,
         -- holding ("h") each message until the start of the next
         -- message with the same label,
         -- and adding the message on=1 ("o") to every message
         -- in which the "on" parameter is not mentioned.
  $ pre2 "" -- Give each message the same label, the empty string.
            -- (This is therefore a monophonic pattern.)
  [ (0,   m1 "freq" 0) -- play frequency 0 from time 0 to time 1/2.
    -- These "frequencies" are (downstream) used as scale steps,
    -- not as Hz. values.
  , (1/2, m1 "freq" 1) -- play frequency 1 from time 1/2 to time 1
  , (1,   m1 "freq" 2) -- etc.
  , (2,   m1 "freq" 3) ]

-- A scale is used to convert scale step values into halfstep values.
scalePat = slow 4 $ -- This gives the `Museq` a duration of 16 instead of 2.
  mmh 2 $ pre2 "a"  -- Make a `Museq` of scales, of duration 2,
  [ ( 0                    -- Starting at time 0,
    , maj )                -- use the major scale.
  , ( 1                    -- Starting at time 1,
    , [0,1,3,5,7,8,10] ) ] -- play whatever scale this is. (It's phrygian.)

render =
  (<$>) (Note Boop) -- Send it to the Boop synth.
  . hsToHz 300 -- Convert each "freq" value from halfsteps to Hz, such that
               -- 0 halfsteps -> 300 Hz, 12 halfsteps -> 600 Hz, etc.
  . scale 12 scalePat -- Convert each scale 12 step value to halfsteps.

chAll $   -- Change all voices at once -- i.e. play this, and only this.
  mfl     -- Shorthand for Data.Map.fromList.
  [ ( "1" -- Send the following `Museq` to voice 1.
    , render scaleStepPattern)
  , ( "2"
    , render $
      freq (+2) $ -- Play it a "musical third" (two 0-indexed scale spaces)
                  -- higher than it would otherwise be.
      fast 2 $    -- Play it twice as fast.
      scaleStepPattern)
  , ( "lizard mango", -- Voice names can be whatever you want.
      render $ freq (+4) $
      fast 4 scaleStepPattern) ]
