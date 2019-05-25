-- simpler patterns
s1  = mmrt1 2 $ map (_1 %~ RTime) $  [ (0, SampleKm), (1, SampleKt) ]
s13 = mmrt1 2 $ map (_1 %~ RTime) $  [ (0, SampleKm), (5/3, SampleKt) ]
s2  = mmrt1 3 $ map (_1 %~ RTime) $  [ (0, SampleHl_et), (1, SampleSp_t) ]

viewDurs = vec .~ mempty

chAll $ mfl
  [
    ("2", fast 2 $ stack [ cat [ sparse 2 $ cat [ mempty
                                                , early (1/6) $ dense 2 s2 ]
                               , dense 3 s2 ]
                         , cat [ s13, s1, dur .~ 2 $ mempty, s1
                               ] ] )
  ]
