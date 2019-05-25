-- the "kick kick snare silence" pattern (ala "we will rock you")
ks = fast 4 $ -- causes `ks` to have a duration of 1
  mmrt1 4 $ -- "mmrt" makes a Museq ("mm") and adds retrigger ("rt")
       -- messages shortly after each note. (SuperCollider needs those
       -- for the sampler to be ready for the next message. In a future
       -- version these messages will be sent automatically.)
  map (_1 %~ RTime) $ -- convert the first thing in each pair from a number
                      -- to an `RTime`
  [ (0, SampleKm)
  , (1, SampleKt)
  , (2, SampleSp_t) ]

hats = fast 4 $ mmrt1 4 $ -- causes hats to have a duration of 1
  map (_1 %~ RTime) $
  [ (0, SampleHl_et)
  , (2, SampleHl_et)
  , (3, SampleHl_et) ]

transformations = mm 4 $ pre3 "a"
  [ (0, 1,   id) -- from time 0 to time 1, this is the identity function
  , (1, 2,   rev) -- from 1 to 2, it is the reversal function
  , (2, 3,   early $ 1/2) -- etc.
  , (3, 3.5, fast 2) -- From 3.5 to 4, there is no transformation.
                     -- The result is silence.
  ]

chAll $ mfl
  [ ("hat",        meta (fast (3/2) transformations) $ fast 2 hats )
  , ("kick snare", meta (slow 4     transformations) ks) ]
