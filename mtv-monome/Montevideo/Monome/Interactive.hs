(mst, quit) <- edoMonome CfgMonome.my46 -- Start synth.
  -- my41_2_11
  -- myKite
  -- my34_thanos
sh aLens = (^. aLens) <$> readMVar mst                       -- show things
ch aLens aFunc = modifyMVar_ mst $ return . (aLens %~ aFunc) -- change things
d :: ZotParam -> Float -> IO () = chDefault mst -- change a parameter
shd = sh stZotDefaults >>= myPrint . M.toList -- show defaults, readably
b :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's floor
  ch (stZotRanges . at p . _Just . _2) $ const r )
t :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's ceiling
  ch (stZotRanges . at p . _Just . _3) $ const r )
sp :: IO () = ( -- store a preset (at Presets.hs)
  readMVar mst >>= storePreset )
lp :: Map ZotParam Float -> IO () = ( \m -> -- load a preset
  ch stZotDefaults $ const m )
hey :: IO () = freeAllVoices mst
