:set prompt "> "
import Text.Megaparsec (parse)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar)
import Data.Either
import Data.Map (size, keys)


reg <- emptySynthRegister
f s = forkIO $ mapM_ act' $ fromRight [] $ parse (msgs reg) "" s

-- | = comma separation works
size <$> readMVar (boops reg) -- should be 0
(Right as) = parse (msgs reg) "" "new boop 3, free boop 3"
act' $ as !! 0
size <$> readMVar (boops reg) -- should be 1
act' $ as !! 1
size <$> readMVar (boops reg) -- should be 0

-- | = new works
(Right as) = parse (msgs reg) "" "new boop 1"
mapM_ act' as
size <$> readMVar (boops reg) -- should be 1

-- | = send works
(Right as) = parse (msgs reg) "" "send boop 1 freq 444.0 amp 0.2"
mapM_ act' as
r <- readMVar (boops reg)
size r

-- | = free works
(Right as) = parse (msgs reg) "" "free boop 1"
mapM_ act' as
r <- readMVar (boops reg)
size r -- at this point it should have size 0

-- | = it all works!
reg <- emptySynthRegister
mapM_ act' $ fromRight [] $ parse (msgs reg) "" "new boop 2, send boop 2 freq 444.0 amp 0.1, wait 1.0, free boop 2"

-- if this gives [], it didn't work
aTest s = fromRight [] $ parse (msgs reg) "" s

-- playing with sqfm
f "new sqfm margaret, send sqfm margaret freq 555.0 amp 0.2 width 0.1 width-vib-freq 22.0 width-vib-amp 0.1, wait 1.0, free sqfm margaret"
