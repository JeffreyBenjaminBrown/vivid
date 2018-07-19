{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.ParseParams (
    parseBoopMsg
  , parseVapMsg -- TODO : unfinished, needs more params
  , parseSqfmMsg
) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib
import Vivid.Jbb.ParseUtils


-- | parse a Msg for a particular synthdef

parseBoopMsg :: Parser (Msg BoopParams)
parseBoopMsg = freq <|> amp

parseVapMsg :: Parser (Msg VapParams)
parseVapMsg = freq <|> amp

parseSqfmMsg :: Parser (Msg SqfmParams)
parseSqfmMsg = freq <|> amp <|> width <|> widthVib


-- | parse a Msg polymorphically

freq :: Elem "freq" superset => Parser (Msg superset)
freq = L.lexeme sc $ do n <- word "freq" >> signedFloat
                        return $ Msg (toI n :: I "freq")

amp :: Elem "amp" superset => Parser (Msg superset)
amp = L.lexeme sc $ do n <- word "amp" >> signedFloat
                       return $ Msg (toI n :: I "amp")

width :: Elem "width" superset => Parser (Msg superset)
width = L.lexeme sc $ do n <- word "width" >> signedFloat
                         return $ Msg (toI n :: I "width")

widthVib :: Elem "width-vib" superset => Parser (Msg superset)
widthVib = L.lexeme sc $ do n <- word "width-vib" >> signedFloat
                            return $ Msg (toI n :: I "width-vib")
