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
-- (5) Run ":. music/<this file's name>" to execute this file, after
--     substituting the actual name of this file between the < and the >.
--
-- If at any point there's some multi-line code that you want to evaluate
-- without interrupting the ongoing GHCI context,
-- you can put it in a file and evaluate ":. path/to/file", just like
-- in step 5 above. (Omit the file extension when doing that.)
-- You could alternatively just rerun ":. music/<this file's name>",
-- but in that case you'll create and use a new dispatch,
-- which will interrupt the music.


:set -XTupleSections

hush -- don't worry if this is not defined
off  -- don't worry if this is not defined

disp <- newDispatch'
tid <- startDispatchLoop' disp
hush = replaceAll' disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program

p f = mkMuseq'ho 3
  [ ("a", 0, M.singleton "freq" f)
  , ("a", 1, M.singleton "freq" $ f*1.2)
  , ("a", 2, M.singleton "on" 0)]

q f = mkMuseq'ho 1
  [ ("a", 0, M.fromList [("freq",f)])
  , ("b", 0, M.fromList [("freq",f*3/2)]) ]

m = mkMuseq' 9
  [ Event "a" (0,3) id
  , Event "a" (3,6) $ fast' 2
  , Event "b" (3,7) $ fast' 4
  -- Wart: For safety, to prevent hanging notes,
  -- both voices must be turned off explicitly, I think.
  , Event "a" (7,9) $ insertOffs
  , Event "b" (7,9) $ insertOffs
  ]

replaceAll' disp $ M.fromList
   [ ("1", Note' Boop <$> meta' m (mergea' (p 1) (q 400))) ]
