-- | How to run this code
--
-- (0) You'll need Stack and SuperCollider installed.
-- (1) Clone this branch of this repo -- for instance, with
--    "git clone -b jbb-update https://github.com/JeffreyBenjaminBrown/vivid/"
--     From Bash, visit its root folder.
-- (2) Run "bash sc-start.sh" to start SuperCollider.
--     (Or start SuperCollider in whatever other way you prefer.)
-- (3) Start GHCI by running "stack ghci".
-- (4) Run ":s init.hs" to set up the environment.
-- (5) Run ":. music/demo" to execute this file.
--
-- If at any point there's some multi-line code that you want to evaluate
-- without interrupting the ongoing GHCI context,
-- you can put it in a file and evaluate ":. path/to/file", just like
-- in step 5 above. (Omit the file extension when doing that.)


:set -XTupleSections


-- | == Some boilerplate to start the program
-- | = first halt anything that might already be running.

hush -- don't worry if this is not defined
off -- don't worry if this is not defined


-- | = Then create new stuff
disp <- newDispatch'
tid <- startDispatchLoop' disp
hush = replaceAll' disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program


-- | = `p f1 f2` defines a simple `Museq`, a note-note-silence pattern.
-- It could control more parameters, but here it only uses "freq" and "amp".

let p :: Float -> Float -> Museq' String Msg
    p f1 f2 = nameAnonEvents'
              $ mkMuseq' 1 -- 1 is the `Museq`'s duration
              $ (\(t,msg) -> mkEv0 Nothing t msg)
              <$> [ (0 -- when the first note (bunch of messages) is sent
                    , M.fromList [ ("freq",f1)
                                 , ("amp",0.1) ] )
                  , (1/3 -- when the second note is sent
                    , M.fromList [ ("freq",f2)
                                 , ("amp",0.1) ] )
                  , (2/3 -- when the synth is silenced
                    , M.singleton "amp" 0) ]

let v :: Float -> Museq' String Msg -- "v" for "verbose"
    v f1 = nameAnonEvents'
              $ mkMuseq' 1 -- 1 is the `Museq`'s duration
              $ (\(s,t,msg) -> mkEv Nothing s t msg)
              <$> [ (0, 1/2 -- when the first note (bunch of messages)
                    , M.fromList [ ("freq",f1)
                                 , ("amp",0.1) ] )
                  , (1/2, 1 -- when the silence
                    , M.fromList [ ("freq", f1)
                                 , ("amp",0) ] ) ]

toBoop = Note' Boop
  -- `Boop` is an enum type that represents the `boop` synthdef.
  -- `Boop "a"` and `Boop "b"` are synths, instances of the Boop synthdef.
  -- A `Msueq` can send to more than one synth; that's why `Museq`s
  -- have names separate from the synths they send to.
  -- (I might remove that feature to simplify things.)

replaceAll' disp
  $ M.fromList [ ("a slow pattern", toBoop <$> p 400 500)
               , ("a faster one", toBoop <$> p 600 700) ]

x = stack'
    (append' (p 600 700)
     (fast' 3 $ early' (1/3) $ p 650 750))
    (mergea' (p 1 $ 2/3) (p 800 900))

y'' =                 stack' (v 100) (early' (1/2) $ v 300)
y' = append' (v 200) (stack' (v 100) (early' (1/2) $ v 300))
y = (mergea' (append' (v 2) (stack' (v 1) (early' (1/2) $ v 3))) (v 100))


replace' disp "a" $ toBoop <$> (append' (v 200) $ stack' (v 100) (early' 0.5 $ v 300))
replace' disp "a" $ toBoop <$> (append' (v 200) $ stack' (v 100) (v 300))
replace' disp "a" $ toBoop <$> stack' (v 100) (v 300)

replace' disp "a" $ toBoop <$> y''

-- replace' disp "a" $ toBoop <$> (mergea' (p 1 $ 2/3) (fast' 5 $ p 800 900))
-- replace' disp "a" $ toBoop <$> mergea' (append' (v 2) (v 3)) (fast' 2 $ v 400)
