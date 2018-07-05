-- Here's a synth with two parameters.

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L

boop :: SynthDef '["freq","amp"]
boop = sd ((0,0.01) -- default values
           :: (I "freq",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

main :: IO ()
main = do
   s <- synth boop ()
   s' <- synth boop ()
   s'' <- synth boop ()
   let freqs = [200,400]
       freqs' = [250,350,450]
       freqs'' = [650,675,725,750,1825,1825,775,1775]
       amps = [0.05,0.1,0.15,0.1,0]
       amps' = [0.05,0.15,0.1,0,0,0,0.1,0]
   forM_ (L.zip5 (cycle freqs) (cycle freqs') (cycle freqs'')
                 (cycle amps) (cycle amps')
         )
     $ \(n,n',n'',a,a') -> do
      set s   (toI n   :: I "freq")
      set s   (toI a   :: I "amp")
      set s'  (toI n'  :: I "freq")
      set s'  (toI a'  :: I "amp")
      set s'' (toI n'  :: I "freq")
      -- s'' will keep its default amp value
      wait 0.1
