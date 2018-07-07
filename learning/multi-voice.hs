{-# LANGUAGE DataKinds #-}

import Vivid

boop :: SynthDef '["note"]
boop = sd (0 :: I "note") $ do
   s1 <- 0.1 ~* sinOsc (freq_ (V::V "note") )
   out 0 [s1, s1]

main :: IO ()
main = do
   s <- synth boop ()
   s' <- synth boop ()
   s'' <- synth boop ()
   let notes = [200,400]
       notes' = [250,350,450]
       notes'' = [650,675,725,750,1825,1825,775,1775]
   forM_ (zip3 (cycle notes) (cycle notes') (cycle notes''))
     $ \(n,n',n'') -> do
      set s   (toI n   :: I "note")
      set s'  (toI n'  :: I "note")
      set s'' (toI n'' :: I "note")
      wait 0.1
