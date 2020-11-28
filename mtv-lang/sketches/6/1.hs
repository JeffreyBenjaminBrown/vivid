p1 = stack2 a b where
  a = mmho 2 $ pre2 "a" [(0, m1 "freq" 0)
                        ,(1, m1 "freq" 2)]
  b = mmho 2 $ pre2 "b" [(0, m1 "on" 0)
                        ,(1, m1 "freq" 4)]

p2 = mmho 6 $ pre2 "a" $ zip (map RTime [0..])
     $ map (m1 "freq" . (-) 0) [0..5]

go = nBoop . toHz . rootScale 12 rs where
  toHz = ops [("freq", (*) 200 . \p -> 2**(p/12))]
  rs = slow 12 $ mmh 2 $ pre2 "a" $ [ (0, (0, phr3))
                                    , (1, (4, lyd))
                                    ]

chAll $ mfl [
  ("1", go $ merge0fa p1 p2)
  , ("2", go $ ops [("freq",(+7))] $ fast 2 $ rev $ merge0fa p1 p2)
  ]
