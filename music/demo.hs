-- | How to run this code
--
-- (0) You'll need Stack and SuperCollider installed.
-- (1) Clone this repo, and (in Bash) visit the root folder of it.
-- (2) From Bash, run "bash sc-start.sh" to start SuperCollider.
--     (Or start SuperCollider in whatever other way you prefer.)
-- (3) From Bash, start GHCI by running "stack ghci". 
-- (4) From GHCI, run ":s init.hs"
-- (5) From GHCI, run ":. music/demo"
--
-- If at any point there's a bunch of code that you want to evaluate
-- without interrupting the ongoing GHCI context,
-- you can put it in a file and evaluate ":. path/to/file", just like
-- in step 5 above.


-- | = Some boilerplate to start the program
disp <- newDispatch
tid <- startDispatchLoop disp
hush = replaceAll disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program


-- | = This defines a simple `Museq`: a pattern that goes note-note-silence.
-- It could control any parameters; here it only uses "freq" and "amp".
let p :: Float -> Float -> Museq Msg
    p f1 f2 = museq' 1 -- 1 is the `Museq`'s duration
      [ (0 -- when the first note (bunch of messages) is sent
        , M.fromList [ ("freq",f1)
                     , ("amp",0.2) ] )
      , (1/3 -- when the second note is sent
        , M.fromList [ ("freq",f2)
                     , ("amp",0.2) ] )
      , (2/3 -- when the synth is silenced
        , M.singleton "amp" 0) ]


-- | = Make some noise!

-- `replaceAll` assigns all `Museq`s in play at once.
-- Here there are 2, but there could be any number.
replaceAll disp
  $ M.fromList [ ("a slow pattern", Send Boop "a" <$> p 400 500)
               , ("a faster one", Send Boop "b" <$> (fast 3 $ p 600 700)) ]
  -- `Boop` is an enum type that represents the `boop` synthdef.
  -- `Boop "a"` and `Boop "b"` are synths, instances of the Boop synthdef.
  -- A `Msueq` can send to more than one synth; that's why `Museq`s
  -- have names separate from the synths they send to.
  -- (I might remove that feature to simplify things.)

-- `replace` a single `Museq` -- in this case, the one named "a faster one".
-- (If there was no such `Museq` before, there is now.)
replace disp "a faster one" $ Send Boop "2" <$>
  (early (1/6) $ fast 2 $ p 500 425)
  -- see Transform.hs for more ways to transform `Museq`s

stop disp "nope" -- Stop and delete the Museq named "nope", if it exists.
  -- (In this case it doesn't, so nothing happens.)

-- `append` and `merge` `Museq`s. (You can also `stack` them, but until
-- polyphonic `Museq`s become more convenient, I can't recommend that.)
let m1 = fast 2 $ append (p 500 900) (p 1000 750)
    m2 = slow 2 $ early (1/3) $ p 1 2
  in replace disp "merge" $ Send Boop "3" <$> mergea m1 m2
    -- see Join.hs for more ways to join `Museq`s

-- use `meta` to apply a `Museq (Museq a -> Museq a)` to a `Museq a`
let f1 = museq 4 [ ((0,2),fast 2)
                 , ((2,4),early $ 1/2) ]
    m2 = slow 2 $ early (1/3) $ p 800 960
  in replace disp "meta" $ Send Boop "4" <$> meta f1 m2

-- Change the tempo. A bigger value = a slower tempo.
chTempoPeriod disp 1.05
