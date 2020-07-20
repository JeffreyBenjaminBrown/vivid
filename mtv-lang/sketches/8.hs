seq evs = f $ zip (map RTime [0..]) evs where
  f [] = []
  f [a] = [a]
  f ((t,ev):b:more) = (t+1/2, ev) : b : f more

patKs = mmt1 4 $ seq [ SampleKm, SampleSm_peb, SampleSl_blip ]
patHat = mmt1 4 $ seq [ SampleHl_et, SampleHl_tfc, SampleHl_tfc ]

-- simpler patterns
s1 = mmt1 2 $ map (_1 %~ RTime) $  [ (0, SampleKm), (1, SampleKt) ]
s2 = mmt1 2 $ map (_1 %~ RTime) $  [ (0, SampleHl_et), (1, SampleSp_t) ]

viewDurs = vec .~ mempty

chAll $ mfl
  [

--    ("1", let x = stack [ patKs
--                         , early 1 $ fast 4 patKs
--                         , fast 4 $
--                           meta ( slow 4 $ mmh 2 $ pre2 "b" $
--                                  seq [ early $ 1/2, late $ 1/2 ] ) $
--                           patHat ]
--          in cat [ stack [sparse 2 x, early 2.75 $ sparse 2 x]
--                 , x ]
--    )
--  ,

    ("2", fast 2 $ stack [ cat [ sparse 2 $ cat [ mempty
                                                 , dense 2 s2 ]
                                , dense 4 s2
                                ]
                          , cat [dur .~ 6 $ s1, slow 2 mempty ] ] )

  ]
