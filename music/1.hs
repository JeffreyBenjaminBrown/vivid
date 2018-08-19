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

dist <- newDispatch
swapMVar (mMuseqs dist) $ M.fromList [ ("1",fast 11 a1), ("2",fast 2 a3)]
swapMVar (mTempoPeriod dist) $ 0.25

s <- synth boop ()
t <- synth boop ()
u <- synth boop ()
swapMVar (mReg dist) $ SynthRegister {
  _boops = M.fromList [("1",s),("2",t),("3",u)]
  , _vaps = M.empty
  , _sqfms = M.empty }

-- replaceAll dist $ M.fromList [ ("1",fast 3 a1), ("2",fast 2 a2)]
-- replaceAll dist $ M.fromList [ ("1",fast 4 a1), ("2",fast 1.5 a3)]
-- replace dist "2" a3

-- TODO ! bug, freezes synths mid-note
-- replaceAll dist $ M.fromList [ ("2",fast 3 a3)]

tryReadMVar $ mMuseqs dist
tryReadMVar $ mReg dist
tryReadMVar $ mTime0 dist
tryReadMVar $ mTempoPeriod dist

tid <- startDispatchLoop dist
putStrLn "dist <- newDispatch\ntid <- startDispatchLoop dist"

off = killThread tid >> freeAll

