{-# LANGUAGE DataKinds #-}

import Vivid

wobble :: SynthDef '["note"]
wobble = sd (0 :: I "note") $ do
   s <- 3 ~* sinOsc (freq_ 10) ? KR
   s1 <- 0.1 ~* sinOsc (freq_ $ (V :: V "note") ~+ s)
   out 0 [s1, s1]

main :: IO ()
main = do
   s <- synth wobble ()
   let notes = take 12 $
         [ x | x <- [200..600], (x `mod` 50) `elem` [0,15,30] ]
   s' <- synth wobble ()
   let notes' = take 12 $
         [ x | x <- [300..700], (x `mod` 50) `elem` [0,40,45] ]
   forM_ (cycle $ zip notes notes') $ \(note,note') -> do
      set s  (toI note  :: I "note")
      set s' (toI note' :: I "note")
      wait 0.2
