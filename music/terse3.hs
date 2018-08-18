a1 = museq 1 [ (0,   Send Boop "1" ("freq",400) )
             , (0,   Send Boop "1" ("amp",0.4)  )
             , (0.5, Send Boop "1" ("amp",0)    ) ]
m1 = museq 2 [ (0,   ("amp",0)    )
             , (0.5, ("freq",500) )
             , (0.5, ("amp",0.4)  ) ]
a2 = Send Boop "2" <$> m1
a3 = fast 2 $ early (1/4) $ museq 1 [ (0,   Send Boop "3" ("freq",600) )
                                    , (0,   Send Boop "3" ("amp",0.4)  )
                                    , (0.5, Send Boop "3" ("amp",0)    ) ]

dist <- newDispatch3
swapMVar (mMuseqs3 dist) $ M.fromList [ ("1",fast 11 a1), ("2",fast 2 a3)]
swapMVar (mTempoPeriod3 dist) $ 0.25

s <- synth boop ()
t <- synth boop ()
u <- synth boop ()
swapMVar (mReg3 dist) $ SynthRegister3 {
  _boops3 = M.fromList [("1",s),("2",t),("3",u)]
  , _vaps3 = M.empty
  , _sqfms3 = M.empty }

-- replaceAll3 dist $ M.fromList [ ("1",fast 3 a1), ("2",fast 2 a2)]
-- replaceAll3 dist $ M.fromList [ ("1",fast 4 a1), ("2",fast 1.5 a3)]
-- replace3 dist "2" a3

-- TODO ! bug, freezes synths mid-note
-- replaceAll3 dist $ M.fromList [ ("2",fast 3 a3)]

tryReadMVar $ mMuseqs3 dist
tryReadMVar $ mReg3 dist
tryReadMVar $ mTime03 dist
tryReadMVar $ mTempoPeriod3 dist

tid <- startDispatchLoop3 dist
putStrLn "dist <- newDispatch\ntid <- startDispatchLoop dist"

off = killThread tid >> freeAll

