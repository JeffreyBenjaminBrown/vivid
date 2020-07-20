drums =   -- This is a drum pattern.
  mmt1 2 -- The 2 here gives it a duration of 2.
  -- "mmt" = "mm" (make a Museq) + "t" (for trigger messages).
  -- It attaches a "trigger=1" message to each sample,
  -- which would be annoying to have to write by hand.
  [ ( 0           -- At time 0,
    , SampleKd)   -- play one of the kick drum samples.
  , ( 1           -- At time 1,
    , SampleSm_m) -- play one of the snare drum samples
  ]

chAll $ -- `chAll` = "change all" = "play the following, and nothing else"
  mfl   -- `mfl` = `Data.Map.fromList`
  [ ( "1"                 -- From a voice named "1",
    , drums)              -- play the `drums` pattern.
  , ( "2"                 -- From a voice named "2",
    , late (1/8) $ fast 4 -- play the same pattern, 4 times as fast
                          -- and 1/8 of a tempo period ("a bar") late
      drums)
  ]
