:set prompt "> "
import Text.Megaparsec (parse)
import Control.Concurrent.MVar (readMVar)
import Data.Map (size, keys)
import Data.Either


reg <- emptySynthRegister

-- | = comma separation works
size <$> readMVar (boops reg) -- should be 0
(Right as) = parse (msgs reg) "" "new boop 3, free boop 3"
act $ as !! 0
size <$> readMVar (boops reg) -- should be 1
act $ as !! 1
size <$> readMVar (boops reg) -- should be 0

-- | = new works
(Right as) = parse (msgs reg) "" "new boop 1"
mapM_ act as
size <$> readMVar (boops reg) -- should be 1

-- | = send works
(Right as) = parse (msgs reg) "" "send boop 1 freq 444.0 amp 0.2"
mapM_ act as
r <- readMVar (boops reg)
size r

-- | = free works
(Right as) = parse (msgs reg) "" "free boop 1"
mapM_ act as
r <- readMVar (boops reg)
size r -- at this point it should have size 0

-- | = it all works!
(Right as) = parse (msgs reg) "" "new boop 2, send boop 2 freq 444.0, wait 1.0, free boop 2"
mapM_ act as
r <- readMVar (boops reg)
size r
