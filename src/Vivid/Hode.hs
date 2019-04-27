module Vivid.Hode where

import           Data.Map (Map)
import qualified Data.Map as M

import Hode.Hode


baseRslt :: Rslt
baseRslt = mkRslt $ M.fromList _baseRslt

_baseRslt :: [(Addr,RefExpr)]
_baseRslt =
  [(0,Phrase' "")
  ,(1,Phrase' "in")
  ,(2,Phrase' "at")
  ,(3,Tplt' [0,1,2,0])
  ,(4,Phrase' "freq")
  ,(5,Tplt' [4,0])
  ,(6,Phrase' "playing")
  ,(7,Tplt' [6,0])
  ,(8,Phrase' "nBoop")
  ,(9,Tplt' [8,0])
  ,(10,Phrase' "mmho")
  ,(11,Tplt' [0,10,0])
  ,(12,Phrase' "pre2")
  ,(13,Tplt' [12,0]) ]

testRslt :: Rslt
testRslt = mkRslt $ M.fromList $ _baseRslt ++ 
  [(14,Rel' (Rel [12,8,13] 3))
  ,(15,Phrase' "playing")
  ,(16,Tplt' [15,0])
  ,(17,Phrase' "nBoop")
  ,(18,Tplt' [17,0])
  ,(19,Phrase' "mmho")
  ,(20,Tplt' [0,19,0])
  ,(21,Phrase' "3")
  ,(22,Phrase' "pre2")
  ,(23,Tplt' [22,0])
  ,(24,Rel' (Rel [8] 23))
  ,(25,Rel' (Rel [21,24] 20))
  ,(26,Rel' (Rel [25] 18))
  ,(27,Rel' (Rel [26] 16))]



--evalParam :: Rslt -> Addr -> Either String (Map String Float)
--evalParam r a = do
  

