-- Here's a synth with two parameters.

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L

boop :: SynthDef '["note","amp"]
boop = sd ((0,0.01) -- default values
           :: (I "note",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "note"))
   out 0 [s1, s1]

main :: IO ()
main = do
   s <- synth boop ()
   s' <- synth boop ()
   s'' <- synth boop ()
   let notes = [200,400]
       notes' = [250,350,450]
       notes'' = [650,675,725,750,1825,1825,775,1775]
       amps = [0.05,0.1,0.15,0.1,0]
       amps' = [0.05,0.15,0.1,0,0,0,0.1,0]
   forM_ (L.zip5 (cycle notes) (cycle notes') (cycle notes'')
                 (cycle amps) (cycle amps')
         )
     $ \(n,n',n'',a,a') -> do
      set s   (toI n   :: I "note")
      set s   (toI a   :: I "amp")
      set s'  (toI n'  :: I "note")
      set s'  (toI a'  :: I "amp")
      set s'' (toI n'  :: I "note")
      -- s'' will keep its default amp value
      wait 0.1
