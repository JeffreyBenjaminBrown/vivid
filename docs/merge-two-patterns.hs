-- This pattern is monophonic, because each note has the same label, "a".
-- It plays four notes.
-- These frequency values are most naturally thought of as absolute,
-- i.e. in Hz.
p = mkMuseqHo 4
  [ ("a", 0,   M.singleton "freq" 500)
  , ("a", 1,   M.singleton "freq" 600)
  , ("a", 2,   M.singleton "freq" 750)
  , ("a", 3.5, M.singleton "freq" 700) ]

-- This pattern is polyphonic, because the notes have different labels.
-- It plays a static chord (some transposition of C-G-D).
-- These frequency values are most naturally thought of as relative,
-- i.e. 2 means "twice the frequency".
q = mkMuseqHo 1
  [ ("a", 0, M.fromList [("freq",1)])
  , ("b", 0, M.fromList [("freq",3/2)])
  , ("c", 0, M.fromList [("freq",9/8)]) ]

-- When you merge the two patterns, the result is what you would get
-- if you replaced each note in `p` by the chord in `q`.
replaceAll disp $ M.fromList
   [ ( "1", Note Boop <$> -- send the pattern to the Boop synth
            merge1 p q ) ] -- (merge1 multiplies all corresponding parameters.
                           -- If you used merge0 instead of merge1,
                           -- the frequencies would be added.)
