seq d e f = [ (0, d)
            , (1, e)
            , (2, d)
            , (2.5, f) ]
patPitch = mmho 3 $ pre2 "a" $
           map (_2 %~ m1 "freq") $ seq 0 1 2
patJump = mmho 3 $ pre2 "b" $
           map (_2 %~ m1 "freq") $ seq 0 6 12
patKs = mmrt1 3 $ seq SampleSm_peb SampleKm SampleKm
patHat = mmrt1 3 $ seq SampleHl_dc SampleHl_tfc SampleHl_tfc

modPat f g = mmh 3 $ pre2 "c"
  [ (0, f . g)
  , (1, id)
  , (2, f)
  , (2.5, g) ]

scalePat = slow 3 $ mmh 8 $ pre2 "d"
  [ ( 0 , (0, maj3) )
  , ( 3 , (-1, loc7) )
  , ( 4 , (2, aol5) )
  , ( 7 , (-1, loc7) )
  ]

toScale = nBoop
          . ops [("freq", (*) 200 . \p -> 2**(p/12))]
          . rootScale scalePat

chAll $ mfl
  [ ("1", meta ( slow 4 $ early 2 $
                 modPat (fast 2) (late 1)) $
          append patKs $ dense 2 patKs)
  , ("2", meta ( slow 8 $
                 modPat (early $ 1/4) (early $ 1/2)) $ 
          meta ( slow 4 $
                 modPat (fast 4 . early 1) (fast 2))
          patHat)
  , ("4.1", toScale $ stack $ -- this takes advantage of a bug
            let p = append patPitch $ rev patPitch in
              [ merge0 (fast 2 patPitch) $
                meta (mmh 4 $ pre2 "e" $ seq (fast 2) (early 1) id) $
                slow 2 $ p
              , merge0 (mm1 $ m1 "freq" 4) $ fast 2 $
                merge0 (fast 2 patJump) $ rev p
              , merge0 (mm1 $ m1 "freq" 9) $ fast 2 $
                rev p
              ] )
  ]
