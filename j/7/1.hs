seq d e f = [ (0, d)
            , (1, e)
            , (1.5, f) ]
meta1  = slow 12 $ mmh 2 $ pre2 "a" $
           seq id (early (1/2)) (slow 2 . rev)
meta2 = slow 4 $ mmh 2 $ pre2 "a" $ seq id
        (\x -> append x $ dur %~ (/2) $ x)
        (early $ 1/4)
meta3 = mmh 2 $ pre2 "a" $
        seq id (early (1/2)) (fast 2)
patPitch k = mmho 2 $ pre2 "a" $
             map (_2 %~ m1 "freq") $ seq 0 k $ 2*k
patKs  = mmrt1 2 $ seq SampleSm_peb SampleKm     SampleKm
patHat = mmrt1 2 $ seq SampleHl_dc  SampleHl_tfc SampleHl_tfc

go = nZot . toHz . rootScale rs where
  toHz = ops [("freq", (*) 200 . \p -> 2**(p/12))]
  rs = slow 8 $ mmh 3 $ pre2 "a" $ [ (0, (0, phr3))
                                   , (1, (4, lyd))
                                   , (2, (1, lyd7)) ]

toDrums = nAmpTo $ defaultAmp * 2.5

zotTones = mergec $
             mmh 2 $ pre2 "a" $ map (_2 %~ mfl)
             [ (0, [("amp",defaultAmp / 2), ("pulse",0.1)
                   , ("sh",1/2) , ("sh-b",0)
                   , ("lpf",3), ("lpf-m",2)
                   ])
             , (0.5, [("rm",0)])
             , (1, [("amp",defaultAmp*1.5), ("pulse",0)
                   , ("sh",-0.25), ("sh-b",-0.5) -- super-weird
                   , ("am",1/3), ("am-f",1/3), ("am-b",1)
                   , ("del",1000)
                   , ("lpf-m",1)
                   ])
             , (1.5, [("rm",1/3), ("rm-b",1/4), ("rm-f",2/3)])
             ]

chAll $ mfl
  [ ("1", go $ meta meta1 $
          slow 2 $ merge0 (fast 3 $ patPitch 1) (patPitch 2))
  , ("2", go $ (\x -> stack x $ amp (*0.7) $ zotTones x) $
          meta meta1 $ merge0 (mm1 $ m1 "freq" 2) $
          merge0 (fast 3 $ patPitch 2) (patPitch 1))

  , ("d1", toDrums $
           meta meta1 $ meta meta2 $ meta meta3 $
           fast 2 $ stack patKs $ fast 2 patHat )
  ]
