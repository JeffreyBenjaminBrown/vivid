seq d e f = [ (0, d)
            , (1, e)
            , (2, d)
            , (2.5, e)
            , (3, f) ]
patPitch = mmho 4 $ pre2 "a" $
           map (_2 %~ m1 "freq") $ seq 0 1 2
patKs = mmrt1 4 $ seq SampleSm_peb SampleKm SampleKm
patHat = mmrt1 4 $ seq SampleHl_dc SampleHl_tfc SampleHl_tfc

modPat f g = mmh 4 $ pre2 "a"
  [ (0, f . g)
  , (1, id)
  , (2, f)
  , (3, g) ]

scalePat = mmh 2 $ pre2 "a"
  [ ( 0 , maj3 )
  , ( 1 , aol3 )
  ]

toScale = nBoop
          . ops [("freq", (*) 200 . \p -> 2**(p/12))]
          . scale scalePat

chAll $ mfl
  [ ("1", meta ( slow 4 $ early 2 $
                 modPat (fast 2) (late 1)) $
          append patKs $ dense 2 patKs)
  , ("2", meta ( slow 8 $
                 modPat (early $ 1/4) (early $ 1/2)) $ 
          meta ( slow 4 $
                 modPat (fast 4 . early 1) (fast 2))
          patHat)
--  , ("3", toScale $
--          stack patPitch $ stack
--          ( merge0 (mm1 $ m1 "freq" 6) $ fast 2 $ rev patPitch )
--          ( merge0 (mm1 $ m1 "freq" 12) $ fast 6 $ rev patPitch ) )
  , ("4", toScale $ stacks
          [ patPitch
          , merge0 (mm1 $ m1 "freq" 6) $ fast 2 $ rev patPitch
          , merge0 (mm1 $ m1 "freq" 12) $ fast 6 $ rev patPitch ] )
  ]
