module Vivid.Hode where

import           Data.Map (Map)
import qualified Data.Map as M

import Hode.Hode
import Hode.Util.Misc


baseRslt :: Rslt
baseRslt = mkRslt $ M.fromList _baseRslt

aInAt, aFreq, aPlaying, aNBoop, aMmho, aPre2 :: Addr
aInAt = -3
aFreq = -5
aPlaying = -7
aNBoop = -9
aMmho = -11
aPre2 = -13

_baseRslt :: [(Addr,RefExpr)]
_baseRslt =
  [( 0,Phrase' "")
  ,(-1,Phrase' "in")
  ,(-2,Phrase' "at")
  ,(-3,Tplt' [0,-1,-2,0])
  ,(-4,Phrase' "freq")
  ,(-5,Tplt' [-4,0])
  ,(-6,Phrase' "playing")
  ,(-7,Tplt' [-6,0])
  ,(-8,Phrase' "nBoop")
  ,(-9,Tplt' [-8,0])
  ,(-10,Phrase' "mmho")
  ,(-11,Tplt' [0,-10,0])
  ,(-12,Phrase' "pre2")
  ,(-13,Tplt' [-12,0]) ]

evalSynthParam :: Rslt -> Addr -> Either String (Map String Float)
evalSynthParam r a = prefixLeft "evalParam: " $ do
  verifyVariety r a (Just RelCtr, Just 1)
  tplt <- fills r (RoleTplt, a)
  verifyVariety r tplt (Just TpltCtr, Just 1)
  mbr <- fills r (RoleMember 1, a)
  verifyVariety r mbr (Just PhraseCtr, Just 0)
  val0 <- addrToExpr r mbr
  let Phrase val1 = val0
      val2 = read val1 :: Float
  if tplt == aFreq then Right $ M.singleton "freq" val2
    else Left $ "Template of Expr at " ++ show a
         ++ " is not a synth  parameter."

