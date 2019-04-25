:set -XTupleSections

hush -- don't worry if this is not defined
off  -- don't worry if this is not defined

disp <- newDispatch'
tid <- startDispatchLoop' disp
hush = replaceAll' disp M.empty
  -- run "hush" to stop and delete all ongoing loops
off = killThread tid >> freeAll -- run "off" to stop the program
