pd = mmrt 2 [ ("a", 0, SampleKd, mempty)
            , ("a", 1, SampleSm_m, mempty)
            ]

pb = (<$>) (Note Boop) $ mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 220)
  , (1, m1 "freq" 440)
  , (2, m1 "freq" 330)
  , (3, m1 "on"   0) ]

chAll $ mfl [
    ("1", fast 2 pd)
  , ("2", pb)
  ]
