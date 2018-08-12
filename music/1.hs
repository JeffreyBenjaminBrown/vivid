a1 = museq 1 [ (0,   Send Boop "1" ("freq",400) )
             , (0,   Send Boop "1" ("amp",0.4)  )
             , (0.5, Send Boop "1" ("amp",0)    ) ]

m1 = museq 2 [ (0,   ("amp",0)    )
             , (0.5, ("freq",500) )
             , (0.5, ("amp",0.4)  ) ]

a2 = Send Boop "2" <$> m1

dist <- newDistrib
swapMVar (mTimeMuseqs dist) $ M.fromList [ ("1",(0,a1)),
                                           ("2",(0,a2))]
mapM_ (act $ reg dist) $ unique
  $ concatMap newsFromMuseq [a1,a2]
tid <- startDistribLoop dist

