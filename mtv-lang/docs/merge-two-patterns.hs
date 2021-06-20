-- This sketch merges two Museqs by multiplying their frequencies.

-- This pattern is monophonic, because each note has the same label, "a".
-- It plays four notes.
melody = mkMuseq_holdOn 4
  [ ("a", 0,   M.singleton "freq" 250)
  , ("a", 1,   M.singleton "freq" 300)
  , ("a", 2,   M.singleton "freq" 375)
  , ("a", 3.5, M.singleton "freq" 370) ]

-- This pattern is polyphonic, because the notes have different labels.
-- It plays a static chord (some transposition of C-G-D).
-- Since in Hz they would be below the human range of hearing,
-- these frequency values are most naturally thought of as relative,
-- i.e. 2 means "twice the frequency".
arpeggio = -- This has a duration of 1: the `2` arguments
           -- to `fast` and `mm` cancel out.
  fast 2 $
  insertOns $ -- into messages missing an "on" instruction, insert "on=1"
  mm 2
  [ ("a", 0/4, 3/2, M.fromList [("freq",1)]  )
  , ("b", 1/4, 3/2, M.fromList [("freq",3/2)])
  , ("c", 2/4, 3/2, M.fromList [("freq",9/4)])
  , ("a", 3/2,   2, M.fromList [("on",0)] ) -- off messages
  , ("b", 3/2,   2, M.fromList [("on",0)] ) -- off messages
  , ("c", 3/2,   2, M.fromList [("on",0)] ) -- off messages
  ]

-- Here's what the first pattern sounds like.
-- chAll $ mfl [ ( "1", Note Boop <$> melody ) ]

-- Here's what the second pattern sounds like (pitched up by a factor of 400).
-- chAll $ mfl [ ( "1", Note Boop <$> overParams [("freq", (*400))] arpeggio ) ]

-- When you merge the two patterns, the result is what you would get
-- if you replaced each note in `melody` by the arpeggio in `arpeggio`.
chAll $ mfl
   [ ( "1", Note Boop <$>  -- Send the pattern to the Boop synth.
            merge1 melody arpeggio ) ]
   -- (`merge1` multiplies like parameters.
  -- If you used merge0 instead of merge1, the frequencies
  -- from the two patterns would be added instead of multiplied.)
