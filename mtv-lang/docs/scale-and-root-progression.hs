-- This is a slight variation on "scale-progression.hs";
-- anything not documented here might be documented there.

scaleStepPattern =
  mmho 3
  $ pre2 "a"
  [ (0,   m1 "freq" 0)
  , (1/2, m1 "freq" 1)
  , (1,   m1 "freq" 2)
  , (2,   m1 "freq" 3) ]

rootScalePat = slow 4 $ 
  mmh 2 $ pre2 "a" 
  [ ( 0           -- Starting at time 0,
    , (0, dor7) ) -- use dorian b7 (a.k.a. harmonic minor) rooted at 0.
  , ( 1             -- Starting at time 1,
    , (3, lyd7) ) ] -- use lydian b7 rooted at 3.
  -- The root of the second scale is 3 halfsteps above the root of the first.

render = (<$>) (Note Boop)
  . ops [( "freq"
         , (*) 300 . \p -> 2**(p/12) )]
  . rootScale 12 rootScalePat

chAll $ mfl
  [ ( "1"
    , render scaleStepPattern)
  , ( "2"
    , render $
      freq (+2) $
      fast 2 $
      scaleStepPattern)
  , ( "mushy bongo sasquatch",
      render $ freq (+4) $ fast 4 $ scaleStepPattern) ]
