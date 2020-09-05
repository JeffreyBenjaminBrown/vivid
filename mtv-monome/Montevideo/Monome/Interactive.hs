(mst, quit) <- edoMonome my46 -- start
  -- You can now play the monome while still using GHCI normally.
-- Define some shorthand for showing and changing things.
sh aLens = (^. aLens) <$> readMVar mst
ch aLens aFunc = modifyMVar_ mst $ return . (aLens %~ aFunc)
-- TODO the following should affect held notes, not just new ones
d :: ZotParam -> Float -> IO () = ( \p f -> -- change a default
  ch stZotDefaults $ M.insert p f )
b :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's floor
  ch (stZotRanges . at p . _Just . _2) $ const r )
t :: ZotParam -> Rational -> IO () = ( \p r -> -- change a range's ceiling
  ch (stZotRanges . at p . _Just . _3) $ const r )

-- Read portions of the St like this:
sh stZotDefaults
sh $ stZotRanges . at Zot_fm_m
