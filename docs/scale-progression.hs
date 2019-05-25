scaleStepPattern =
  mmho 3 -- make a Museq ("mm") of duration 3,
         -- holding ("h") each message until the next one starts,
         -- and adding the message on=1 ("o") to every message
  $ pre2 "a" -- give each message the same label
             -- (so each goes to the same place, cutting off the previous)
  [ (0,   m1 "freq" 0) -- play frequency 0 from time 0 to time 1/2
  , (1/2, m1 "freq" 1) -- play frequency 1 from time 1/2 to time 1
  , (1,   m1 "freq" 2) -- etc.
  , (2,   m1 "freq" 3) ]

-- A scale is used to convert scale step values into halfstep values.
scalePat = slow 4 $ -- this gives the Museq a duration of 16 instead of 2
  mmh 2 $ pre2 "a" -- make a Museq of scales, of duration 2
  [ ( 0     -- starting at time 0,
    , maj ) -- use the major scale
  , ( 1                    -- starting at time 1,
    , [0,1,3,5,7,8,10] ) ] -- play whatever scale this is. (it's phrygian.)

render = (<$>) (Note Boop) -- send it to the Boop synth
  . ops [( "freq" -- convert each "freq" value from halfsteps to Hz, where
                  -- 0 halfsteps = 300 Hz, 12 halfsteps = 600 Hz, etc.
         , (*) 300 . \p -> 2**(p/12) )]
  . scale scalePat -- convert each scale step value to halfsteps

chAll $ -- change all voices at once -- i.e. play this, and only this
  mfl -- shorthand for Data.Map.fromList
  [ ( "1" -- send this to voice 1
    , render scaleStepPattern)
  , ( "2"
    , render $
      freq (+2) $ -- play it a "musical third" (i.e. 2 0-indexed scale spaces)
                  -- higher than it would otherwise be
      fast 2 $ -- and play it twice as fast
      scaleStepPattern)
  , ( "lizard", -- voice names can be whatever you want
      render $ freq (+4) $ fast 4 $ scaleStepPattern) ]
