hush -- don't worry if this is not defined
putStrLn "If hush just produced an error, don't worry, it's cool."
off  -- don't worry if this is not defined
putStrLn "If off just produced an error, don't worry, it's cool."

-- Hopefully you'll never need to use these explicitly.
disp <- newDispatch
tid <- startDispatchLoop disp
off = killThread tid >> freeAll -- kill the program

-- These, though, you'll use a lot.
ch = replace disp                        -- change one thing
chAll = replaceAll disp                  -- change everything
stop = Montevideo.Dispatch.Dispatch.disp -- stop (and lose) one thing
hush = replaceAll disp M.empty           -- stop (and lose) everything
period = chTempoPeriod disp
