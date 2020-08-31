(mst, quit) <- edoMonome my46 -- start
  -- You can now play the monome while still using GHCI normally.

-- Define some shorthand for showing and changing things.
sh aLens = (^. aLens) <$> readMVar mst
ch aLens aFunc = modifyMVar_ mst $ return . (aLens %~ aFunc)

-- Read portions of the St like this:
sh stZotDefaults
sh $ stZotRanges . at Zot_fm_m

-- Set things like this:
ch stZotDefaults $ M.insert Zot_fm_f $ 2**(-16)
ch (stZotRanges . at Zot_fm_m . _Just . _3) $ const 3
