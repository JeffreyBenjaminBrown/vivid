-- everything below includes per-synth boilerplate

{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.DistribLang.Early.Parse.Params (
    parseBoopMsg
  , parseVapMsg -- TODO : unfinished, needs more params
  , parseSqfmMsg
) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Vivid
import Vivid.Jbb.DistribLang.Early.Distrib
import Vivid.Jbb.DistribLang.Early.Msg
import Vivid.Jbb.DistribLang.Early.Parse.Utils
import Vivid.Jbb.Synths


-- | parse a Msg for a particular synthdef

parseBoopMsg :: Parser (MsgEarly BoopParams)
parseBoopMsg = tryEach [freq, amp]

parseVapMsg :: Parser (MsgEarly VapParams)
parseVapMsg = tryEach [freq, amp]

parseSqfmMsg :: Parser (MsgEarly SqfmParams)
parseSqfmMsg = tryEach [freq, amp, width, widthVibFreq, widthVibAmp]


-- | parse a Msg polymorphically

freq :: Elem "freq" superset => Parser (MsgEarly superset)
freq = L.lexeme sc $ do n <- word "freq" >> signedFloat
                        return $ MsgEarly (toI n :: I "freq")

amp :: Elem "amp" superset => Parser (MsgEarly superset)
amp = L.lexeme sc $ do n <- word "amp" >> signedFloat
                       return $ MsgEarly (toI n :: I "amp")

width :: Elem "width" superset => Parser (MsgEarly superset)
width = L.lexeme sc $ do n <- word "width" >> signedFloat
                         return $ MsgEarly (toI n :: I "width")

widthVibFreq :: Elem "width-vib-freq" superset => Parser (MsgEarly superset)
widthVibFreq = L.lexeme sc $ do n <- word "width-vib-freq" >> signedFloat
                                return $ MsgEarly (toI n :: I "width-vib-freq")

widthVibAmp :: Elem "width-vib-amp" superset => Parser (MsgEarly superset)
widthVibAmp = L.lexeme sc $ do n <- word "width-vib-amp" >> signedFloat
                               return $ MsgEarly (toI n :: I "width-vib-amp")
