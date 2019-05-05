evs = [ (0, SampleHl_cg) ]
dPat = mmrt1 1 $ map (_2 %~ id) evs
dp2 = mmrt1 2 [ (0, SampleKd)
              , (1, SampleSp_t) ]

-- chAll $ mfl [ -- 1 and 2 together should sound the same as 3 alone
--   ("1", dPat)
--   , ("2", dp2)
-- --  ("3", stack dp2 dPat)
--   ]

chAll $ mfl [ -- 1 should sound like 2 except louder
  ("1", nMerge1 (mm1 $ m1 "amp" 0.1) $ stack dp2 dPat)
--  ("2",                                stack dp2 dPat)
  ]
