import System.Random

-- | There are 94 Unicode characters bewteen ! and ~ (inclusive).
-- The chance of collision between two 32 character strings of those
-- is (1/94)**32 = 7.242836554608488e-64
randomString :: IO [Char]
randomString = do
   gen <- newStdGen
   return $ Prelude.take 32 $ randomRs
                              ('!','~') -- widest possible on normal keyboard
                              gen
