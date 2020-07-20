seq evs = f $ zip (map RTime [0..]) evs where
  f [] = []
  f [a] = [a]
  f (a:(t,ev):more) = a : (max 0 $ t-1/2, ev) : f more

patKs = mmt1 4 $ seq [ SampleKm
                      , SampleSm_peb
                      , SampleSl_blip ]
patHat = nAmpTo 0.04 $ mmt1 4 $ seq [ SampleHl_et
                                     , SampleHl_tfc
                                     , SampleHl_tfc ]

patFreq = mmho 4 $ [
    (""  , 0, mfl [("freq",60)])
  , (""  , 1, mfl [("freq",70)])
  , (""  , 2, mfl [("freq",70.1)])
  , (""  , 3, mfl [("freq",90)])
  ]

-- When first playing this file, for some reason,
-- patTone has to be really simple.
-- Then it can be redefined as the complex one.
-- But if it is that to start, the synth diverges (loudly).
patTone = fast 4 $ mmh 2 $ pre2 "" [
    (0, mfl [
          ("am-b",0.1), ("fm-b",0)
        , ("del",3/2)
        , ("amp",0.02)
        , ("lpf",55),("lpf-m",1)
      ])
    , (1, mfl [
          ("am-f",1), ("am",20), ("am-b",1/8)
          , ("fm-f",1), ("fm-f",1/2), ("fm-b",1)
          , ("lpf-m",0)
          , ("amp",0.01)
          ])
    ]
--patTone = fast 4 $ mmh 2 $ pre2 "" [(0, mfl [("freq",1)])]

viewDurs = vec .~ mempty
woop x = cat [early 1 x, rev x]

chAll $ mfl [
    ("1", fast 2 $ woop patKs)
  , ("2", early (1/4) $ fast 4 $ patHat )
  , ("3", nZot $ merge1 (woop $ freq (/ 75) patFreq) $
       stack [ patFreq
             , merge1 patTone patFreq
             , early (1/4) $
               merge0 (early (1/8) $ fast 4 patFreq) $
               merge1 (late (1/4) patTone) patFreq
             ] )
  ]
