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


-- | = Some boilerplate to start the program
disp <- newDispatch'
tid <- startDispatchLoop' disp
hush = replaceAll' disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program


-- | = `p f1 f2` defines a simple `Museq`, a note-note-silence pattern.
-- It could control more parameters, but here it only uses "freq" and "amp".

let p :: Float -> Float -> Museq' String Note'
    p f1 f2 = nameAnonEvents'
              $ museq' 1 -- 1 is the `Museq`'s duration
              $ (\(t,msg) -> ev0 Nothing t $ Note' Boop msg)
              <$> [ (0 -- when the first note (bunch of messages) is sent
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
 

replaceAll' disp
  $ M.fromList [ ("a slow pattern", p 400 500)
               , ("a faster one", p 600 700) ]

