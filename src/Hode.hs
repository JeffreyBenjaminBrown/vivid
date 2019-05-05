{-# LANGUAGE ScopedTypeVariables #-}

module Hode where

import qualified Data.Map as M
import qualified Data.Set as S

import Hode.Hode hiding (name)

import Dispatch.Abbrevs
import Dispatch.Dispatch
import Dispatch.Types


baseRslt :: Rslt
baseRslt = mkRslt $ M.fromList _baseRslt

aWhenPlays, aFreq, aSends, aNBoop, aMmho, aPre2 :: Addr
aWhenPlays = -3
aFreq = -5
aSends = -7
aNBoop = -9
aMmho = -11
aPre2 = -13

aParamTplts :: [Addr]
aParamTplts = [aFreq]

aToSynths :: [Addr]
aToSynths = [aNBoop]

_baseRslt :: [(Addr,RefExpr)]
_baseRslt =
  [( 0,Phrase' "")
  ,(-1,Phrase' "when")
  ,(-2,Phrase' "plays")
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
  ,(-13,Tplt' [-12,0])
  ,(-14,Phrase' "sends")
  ,(-15,Tplt' [0,-14,0]) ]

phraseToString :: Rslt -> Addr -> Either String String
phraseToString r a =
  prefixLeft ("phraseToFloat at " ++ show a ++ ": ") $ do
  verifyVariety r a (Just PhraseCtr, Just 0)
  val0 <- addrToExpr r a
  let Phrase val1 = val0
  Right val1

phraseToFloat :: Rslt -> Addr -> Either String Float
phraseToFloat r a =
  prefixLeft ("phraseToFloat at " ++ show a ++ ": ") $ do
  verifyVariety r a (Just PhraseCtr, Just 0)
  val0 <- addrToExpr r a
  let Phrase val1 = val0
  Right (read val1 :: Float)

evalSynthParam :: Rslt -> Addr -> Either String Msg
evalSynthParam r a =
  prefixLeft ("evalParam at " ++ show a ++ ": ") $ do
  fits <- let h = HMap $ M.singleton RoleTplt $ HOr $
                map (HExpr . Addr) aParamTplts
          in hMatches r h a
  if fits then Right () else Left $
    "Is not a unary parameter relationship (e.g. \"#freq 440\".)"
  param <- subExpr r  a [RoleTplt, RoleMember 1]
           >>= phraseToString r
  value <- subExpr r  a [RoleMember 1]
           >>= phraseToFloat r
  Right $ M.singleton param value

-- | It works! Try, e.g., `playSong disp testRslt 10`
evalParamEvent :: Rslt -> Addr -> Either String (String, RTime, Msg)
evalParamEvent r a =
  prefixLeft ("evalParamEvent at " ++ show a ++ ": ") $ do
  fits <- let h = HMap $ M.singleton RoleTplt $ HOr $
                [HExpr $ Addr aWhenPlays]
          in hMatches r h a
  if fits then Right () else Left $
    "Is not a unary parameter relationship (e.g. \"#freq 500\"."

  name :: String <- fills r (RoleMember 1, a)
                    >>= phraseToString r
  time :: Float <- fills r (RoleMember 2, a)
                   >>= phraseToFloat r
  msg :: Msg <- fills r (RoleMember 3, a)
                >>= evalSynthParam r
  Right (name, RTime $ toRational time, msg)

evalEventTriples :: Rslt -> Addr -> Either String [(String, RTime, Msg)]
evalEventTriples r a =
  prefixLeft ("evalEventTriples at " ++ show a ++ ": ") $ do
  hosts <- (<$>) S.toList $ hExprToAddrs r mempty $
    HMap $ M.fromList [ (RoleTplt, HExpr $ Addr aWhenPlays)
                      , (RoleMember 1, HExpr $ Addr a) ]
  ifLefts $ map (evalParamEvent r) hosts

evalMmho :: Rslt -> Addr -> Either String (Museq String Msg)
evalMmho r a =
  prefixLeft ("evalMmho at " ++ show a ++ ": ") $ do
  fits <- let h = HMap $ M.singleton RoleTplt $ HOr $
                map (HExpr . Addr) [aMmho]
          in hMatches r h a
  if fits then Right () else Left $
    "Is not a binary mmho relationship (e.g. \"3 #mmho a\", which would say that the pattern named (a) has a duration of 3."

  dur0 :: Float <- fills r (RoleMember 1, a)
                   >>= phraseToFloat r
  triples :: [(String, RTime, Msg)] <- fills r (RoleMember 2, a)
                                       >>= evalEventTriples r
  Right $ mmho (RTime $ toRational dur0) triples

evalToSynths :: Rslt -> Addr -> Either String (Museq String Note)
evalToSynths r a =
  prefixLeft ("evalToSynths at " ++ show a ++ ": ") $ do
  let misfit = "Is not a unary send-to-this-synth relationship (e.g. \"#nBoop (3 #mmho a)\", which would send to the Boop synth the pattern named (a) with a duration of 3."

  fits <- let h = HMap $ M.singleton RoleTplt $ HOr $
                map (HExpr . Addr) aToSynths
          in hMatches r h a
  if fits then Right () else Left misfit

  pat :: Museq String Msg <- fills r (RoleMember 1, a)
                             >>= evalMmho r
  synth :: String <- subExpr r  a [RoleTplt, RoleMember 1]
                     >>= phraseToString r
  case synth of
    "nBoop" -> Right $ nBoop pat
    _ -> Left misfit

playSong :: Dispatch -> Rslt -> Addr -> IO ()
playSong d r a = let
  museqs :: Either String (M.Map String (Museq String Note)) =
    prefixLeft ("evalToSynths at " ++ show a ++ ": ") $ do
    museqAddrs :: [Addr] <- (<$>) S.toList $
      hExprToAddrs r mempty $
      HEval (HMap $ M.fromList [ (RoleTplt, HExpr $ Addr aSends)
                               , (RoleMember 1, HExpr $ Addr a) ] )
      [[RoleMember 2]]
    x :: [Museq String Note] <-
      ifLefts $ map (evalToSynths r) museqAddrs
    Right $ M.fromList $ zip (map show [1::Int ..]) x
  in case museqs of Left s   -> putStrLn s
                    Right ms -> replaceAll d ms
