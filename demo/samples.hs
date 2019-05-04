pd = mmrt 1 $ pre2 "a"
  [ (0, Note (Sampler "kd") $ mempty )
  ]

pb = (<$>) (Note Boop) $ mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 220)
  , (1, m1 "freq" 440)
  , (2, m1 "freq" 330)
  , (3, m1 "on"   0) ]

chAll $ mfl [
    ("1", pd)
  , ("2", pb)
  ]
