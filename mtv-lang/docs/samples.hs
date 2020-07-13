drums =   -- This is a drum pattern.
  mmrt1 2 -- The 2 here gives it a duration of 2.
  -- "mmrt" = "mm" (make a Museq) + "rt" (insert retrigger messages).
  -- The "retrigger" messages are annoying, and will eventually go away,
  -- but currently one needs to make sure the pattern includes messages
  -- that tell SuperCollider to be ready to retrigger the sampler.
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
