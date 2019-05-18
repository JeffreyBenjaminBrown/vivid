-- For profiling purposes.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Concurrent (killThread)
import qualified Data.Map as M

import Vivid
import VividDispatch


main :: IO ()
main = do
  disp <- newDispatch
  tid <- startDispatchLoop disp

  let -- useful for live coding, but unused in this program
      -- ch = replace disp               -- change one thing
      -- stop = stopDispatch disp        -- stop (and lose) one thing
      -- period = chTempoPeriod disp

      chAll = replaceAll disp         -- change everything
      hush = replaceAll disp M.empty  -- stop (and lose) everything
      off = killThread tid >> freeAll -- kill the program


      p1 = stack a b & dur .~ 4 where
        a = mmho 3 $ pre2 "a" [(0, m1 "freq" 0)
                              ,(1, m1 "freq" 2)]
        b = mmho 2 $ pre2 "b" [(0, m1 "on" 0)
                              ,(1, m1 "freq" 4)]

      run :: Int -> Museq String Msg = \k ->
        mmho (fromIntegral k) $ pre2 "a" $
        zip (map RTime [0..]) $
        map (M.singleton "freq" . (+) 0) $
        map fromIntegral [0..k-1]
      
      chord :: [Float] -> Museq String Msg =
        mmho 1 . pre2 "a" .
        map (\f -> (0,M.singleton "freq" f))
      
      ampSeq :: Float -> Museq String Msg = \f ->
        mmho 1 [("a",0,m1 "amp" f)]
      
      go = nBoop . toHz . rootScale rs where
        toHz = ops [("freq", (*) 200 . \p -> 2**(p/12))]
        rs = slow 12 $ mmh 3 $ pre2 "a" $ [ (0, (0, phr3))
                                          , (1, (4, lyd))
                                          , (2, (1, lyd7))
                                          ]
      
      wave = slow 4 $ mmh 2 $ pre2 "a" $ zip (map RTime [0..]) $
             [id, early $ 1]
      
  chAll $ mfl [
    ("1", go $ fast 2 $ merge0fa (slow 4 $ run 4) $ merge0fa (run 4) p1)
    , ("2", go $ fast 4 $ meta wave $
            merge0fa (slow 8 $ run 4) $ merge0fa (run 3) $
            cat [p1, dense 2 p1])
    , ("3", go $ merge0fa (merge0 (ampSeq 0.001) $ chord [10,12]) $
            merge0fa (slow 4 $ run 4) $ merge0fa (run 4) p1)
    ]

  _ :: String <- readLn
  hush
  off
  wait 1 -- give "hush" and "off" time to work
