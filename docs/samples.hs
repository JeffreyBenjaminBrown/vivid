-- a drum pattern
drums = mmrt1 2 -- the `drums` pattern has a duration of 2.
  -- "mmrt" = "mm" (make a Museq) + "rt" (insert retrigger messages).
  -- The "retrigger" messages are annoying, and will eventually go away,
  -- but currently one needs to explicitly make sure there are messages
  -- that tell SuperCollider to be ready to retrigger the sampler.
  [ (0, SampleKd)
  , (1, SampleSm_m)
  ]

chAll $ mfl
  [ ("1", drums)
  , ("2", late (1/8) $ fast 4 drums)
  ]
