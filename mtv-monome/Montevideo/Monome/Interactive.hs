(mst, quit) <- edoMonome my46 -- Start the synth.
sh aLens = (^. aLens) <$> readMVar mst                       -- show things
ch aLens aFunc = modifyMVar_ mst $ return . (aLens %~ aFunc) -- change things
d :: ZotParam -> Float -> IO () = chDefault mst -- change a parameter
b :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's floor
  ch (stZotRanges . at p . _Just . _2) $ const r )
t :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's ceiling
  ch (stZotRanges . at p . _Just . _3) $ const r )
