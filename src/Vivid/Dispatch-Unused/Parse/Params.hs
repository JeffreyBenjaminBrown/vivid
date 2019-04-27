-- everything below includes per-synth boilerplate

{-# LANGUAGE DataKinds #-}

module Vivid.Dispatch.Parse.Params (
    parseBoopMsg
  , parseVapMsg -- TODO : unfinished, needs more params
  , parseSqfmMsg
) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Vivid
import Vivid.Dispatch.Act
import Vivid.Dispatch.Types
import Vivid.Dispatch.Parse.Utils
import Vivid.Synths


-- | parse a Msg for a particular synthdef

parseBoopMsg :: Parser (Msg' BoopParams)
parseBoopMsg = tryEach [freq, amp]

parseVapMsg :: Parser (Msg' VapParams)
parseVapMsg = tryEach [freq, amp]

parseSqfmMsg :: Parser (Msg' SqfmParams)
parseSqfmMsg = tryEach [freq, amp, width, widthVibFreq, widthVibAmp]


-- | parse a Msg polymorphically

freq :: Elem "freq" superset => Parser (Msg' superset)
freq = L.lexeme sc $ do n <- word "freq" >> signedFloat
                        return $ Msg' (toI n :: I "freq")

amp :: Elem "amp" superset => Parser (Msg' superset)
amp = L.lexeme sc $ do n <- word "amp" >> signedFloat
                       return $ Msg' (toI n :: I "amp")

width :: Elem "width" superset => Parser (Msg' superset)
width = L.lexeme sc $ do n <- word "width" >> signedFloat
                         return $ Msg' (toI n :: I "width")

widthVibFreq :: Elem "width-vib-freq" superset => Parser (Msg' superset)
widthVibFreq = L.lexeme sc $ do n <- word "width-vib-freq" >> signedFloat
                                return $ Msg' (toI n :: I "width-vib-freq")

widthVibAmp :: Elem "width-vib-amp" superset => Parser (Msg' superset)
widthVibAmp = L.lexeme sc $ do n <- word "width-vib-amp" >> signedFloat
                               return $ Msg' (toI n :: I "width-vib-amp")
