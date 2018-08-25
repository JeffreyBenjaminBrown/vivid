a1 = museq' 1 [ (0,   Send Boop "1" ("freq",1400) )
              , (0,   Send Boop "1" ("amp",0.4)  )
              , (0.5, Send Boop "1" ("amp",0)    ) ]
m1 = museq' 2 [ (0,   ("amp",0)    )
              , (0.5, ("freq",1500) )
              , (0.5, ("amp",0.4)  ) ]
a2 = Send Boop "2" <$> m1
a3 = fast 2 $ early (1/4) $ museq' 1 [ (0,   Send Boop "3" ("freq",800) )
                                     , (0,   Send Boop "3" ("amp",0.4)  )
                                     , (0.5, Send Boop "3" ("amp",0)    ) ]

disp <- newDispatch
swapMVar (mMuseqs disp) $ M.fromList [ ("1",fast 11 a1), ("2",fast 2 a3)]
swapMVar (mTempoPeriod disp) 2

s <- synth boop ()
t <- synth boop ()
u <- synth boop ()
swapMVar (mReg disp) $ SynthRegister {
  _boops = M.fromList [("1",s),("2",t),("3",u)]
  , _vaps = M.empty
  , _sqfms = M.empty }

-- replaceAll disp $ M.fromList [ ("1",fast 3 a1), ("2",fast 2 a2)]
-- replaceAll disp $ M.fromList [ ("1",early 1 $ fast 4 a1), ("2",rev $ fast 1.5 a3)]
-- replace disp "2" a3

-- TODO ! bug, freezes synths mid-note
-- replaceAll disp $ M.fromList [ ("2",fast 3 a3)]

-- testing chTempoPeriod
-- replaceAll disp $ M.fromList [ ("1", a1), ("2",  a2)]
-- chTempoPeriod disp 1.05

tryReadMVar $ mMuseqs disp
tryReadMVar $ mReg disp
tryReadMVar $ mTime0 disp
tryReadMVar $ mTempoPeriod disp

tid <- startDispatchLoop disp
putStrLn "disp <- newDispatch\ntid <- startDispatchLoop disp"

off = killThread tid >> freeAll
