-- | How to run this code
--
-- (0) You'll need Stack and SuperCollider installed.
-- (1) Clone this branch of this repo -- for instance, with
--    "git clone -b jbb-update https://github.com/JeffreyBenjaminBrown/vivid/"
--     From Bash, visit its root folder.
-- (2) Run "bash sc-start.sh" to start SuperCollider.
--     (Or start SuperCollider in whatever other way you prefer.)
-- (3) In another terminal at the same folder,
--     start GHCI by running "stack ghci".
-- (4) Run ":s init.hs" to set up the environment.
-- (5) Run ":. music/demo" to execute this file.
--
-- If at any point there's some multi-line code that you want to evaluate
-- without interrupting the ongoing GHCI context,
-- you can put it in a file and evaluate ":. path/to/file", just like
-- in step 5 above. (Omit the file extension when doing that.)


:set -XTupleSections

-- | = Some boilerplate to start the program
disp <- newDispatch
tid <- startDispatchLoop disp
hush = replaceAll disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program


-- | = `p f1 f2` defines a simple `Museq`, a note-note-silence pattern.
-- It could control more parameters, but here it only uses "freq" and "amp".

let p :: Float -> Float -> Museq Note
    p f1 f2 = nameAnonEvents
              $ (Nothing,) . (Boop,)
              <$> mkMuseq0 1 -- 1 is the `Museq`'s duration
              [ (0 -- when the first note (bunch of messages) is sent
                , M.fromList [ ("freq",f1)
                             , ("amp",0.2) ] )
              , (1/3 -- when the second note is sent
                , M.fromList [ ("freq",f2)
                             , ("amp",0.2) ] )
              , (2/3 -- when the synth is silenced
                , M.singleton "amp" 0) ]
  -- `Boop` is an enum type that represents the `boop` synthdef.
  -- `Boop "a"` and `Boop "b"` are synths, instances of the Boop synthdef.
  -- A `Msueq` can send to more than one synth; that's why `Museq`s
  -- have names separate from the synths they send to.
  -- (I might remove that feature to simplify things.)
 
-- -- | = Make some noise!
-- 
-- `replaceAll` assigns all `Museq`s in play at once.
-- Here there are 2, but there could be any number.
replaceAll disp
  $ M.fromList [ ("a slow pattern", p 400 500)
               , ("a faster one", fast 3 $ p 600 700) ]
 
-- `replace` a single `Museq` -- in this case, the one named "a faster one".
-- (If there was no such `Museq` before, there is now.)
replace disp "a faster one" $ (early (1/6) $ fast 2 $ p 500 425)
  -- see Transform.hs for more ways to transform `Museq`s
 
stop disp "nope" -- Stop and delete the Museq named "nope", if it exists.
  -- (In this case it doesn't, so nothing happens.)

---- `append` and `merge` `Museq`s. (You can also `stack` them, but until
---- polyphonic `Museq`s become more convenient, I can't recommend that.)
--let m1 = fast 2 $ append (p 500 900) (p 1000 750)
--    m2 = slow 2 $ early (1/3) $ p 1 2
--  in replace disp "merge" $ mergea m1 m2
--    -- see Join.hs for more ways to join `Museq`s

-- use `meta` to apply a `Museq (Museq a -> Museq a)` to a `Museq a`
let f1 = mkMuseq 4 [ ((0,2),fast 2)
                 , ((2,4),early $ 1/2) ]
    m2 = slow 2 $ early (1/3) $ p 800 960
  in replace disp "meta" $ meta f1 m2

-- Change the tempo. A bigger value = a slower tempo.
chTempoPeriod disp 1.05

replace disp "a funky one, by Tom Murphy" $ cat [fast 4 $ p 330 220, fast 4 $ p 660 440, rev $ p 440 220, rev $ fast 4 $ p 330 440, fast 4 $ p 330 440]
