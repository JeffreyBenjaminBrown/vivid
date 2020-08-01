data SynthRegister' = -- per-synth boilerplate
  SynthRegister' { boops' :: MVar (M.Map SynthName (Synth BoopParams))
                 , vaps'  :: MVar (M.Map SynthName (Synth VapParams))
                 , sqfms' :: MVar (M.Map SynthName (Synth SqfmParams))
                -- , zots :: MVar (M.Map SynthName (Synth ZotParams))
                }

emptySynthRegister' :: IO SynthRegister
emptySynthRegister' = do x <- newMVar M.empty
                         y <- newMVar M.empty
                         z <- newMVar M.empty
--                         w <- newMVar M.empty
                         return $ SynthRegister x y z -- w

-- | todo : this blocks if any MVar is empty
showSynthRegister' :: SynthRegister -> IO String
showSynthRegister' reg = do bs <- show <$> (readMVar $ boops reg)
                            vs <- show <$> (readMVar $ vaps reg )
                            ss <- show <$> (readMVar $ sqfms reg)
                            return $ bs ++ "\n" ++ vs ++ "\n" ++ ss

data Dispatch' = Dispatch' {
  mTimeMuseqs' :: MVar (M.Map MuseqName (Time, Museq ScAction))
    -- ^ Each `Time` here is the next time that Museq is scheduled to run.
    -- Rarely, briefly, those `Time` values will be in the past.
  , reg' :: SynthRegister
  , mTime0' :: MVar Time
  , mTempoPeriod' :: MVar Duration
  }

-- | "new" because it's not really empty, except for `time0`
newDispatch' :: IO Dispatch
newDispatch' = do
  mTimeMuseqs <- newMVar M.empty
  reg <- emptySynthRegister
  mTime0 <- newEmptyMVar
  mTempoPeriod <- newMVar 1
  return Dispatch { mTimeMuseqs = mTimeMuseqs,  reg          = reg
                  , mTime0  = mTime0         ,  mTempoPeriod = mTempoPeriod }

data ScAction' where
  New'  :: MVar (M.Map SynthName (Synth sdArgs))
        -> SynthDef sdArgs
        -> SynthName -> ScAction'
  Free' :: MVar (M.Map SynthName (Synth sdArgs))
        -> SynthName -> ScAction'
  Send' :: MVar (M.Map SynthName (Synth sdArgs))
        -> SynthName
        -> Msg' sdArgs -> ScAction'

