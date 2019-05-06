seq d e f = [ (0, d)
            , (1, e)
            , (2, d)
            , (2.5, e)
            , (3, f) ]
patPitch = mmho 4 $ pre2 "a" $
           map (_2 %~ m1 "freq") $ seq 0 1 2

scalePat = mmh 2 $ pre2 "a"
  [ ( 0 , maj3 )
  , ( 1 , aol3 )
  ]

toScale = nBoop
          . ops [("freq", (*) 200 . \p -> 2**(p/12))]
          . scale scalePat

-- incredibly, iterating `stack` by hand sounds different than `stacks`
chAll $ mfl [
   ("1", toScale $
          stack patPitch $ stack
          ( merge0 (mm1 $ m1 "freq" 6) $ fast 2 $ rev patPitch )
          ( merge0 (mm1 $ m1 "freq" 12) $ fast 6 $ rev patPitch ) )
--   ("2", toScale $ stacks
--          [ patPitch
--          , merge0 (mm1 $ m1 "freq" 6) $ fast 2 $ rev patPitch
--          , merge0 (mm1 $ m1 "freq" 12) $ fast 6 $ rev patPitch ] )
  ]
