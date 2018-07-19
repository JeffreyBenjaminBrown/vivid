{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Parse where

import Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib
import Vivid.Jbb.ParseUtils
import Vivid.Jbb.ParseParams


synthDefName :: Parser SynthDefName
synthDefName = foldr1 (<|>) [ word "boop" >> return Boop
                            , word "vap" >> return Vap
                            , word "sqfm" >> return Sqfm
                            ]

wait :: Parser Action
wait = Wait <$> (word "wait" >> L.lexeme sc L.float)

-- everything below includes per-synth boilerplate

new :: SynthRegister -> Parser [Action]
new reg = do
  word "new"
  synthDef <- synthDefName
  names <- M.many anyWord
  case synthDef of
    Boop -> return $ map (New (boops reg) boop) names
    Vap  -> return $ map (New (vaps  reg) vap ) names
    Sqfm -> return $ map (New (sqfms reg) sqfm) names

free :: SynthRegister -> Parser [Action]
free reg = do
  word "free"
  synthDef <- synthDefName
  names <- M.many $ anyWord
  return $ case synthDef of
    Boop -> map (Free $ boops reg) names
    Vap  -> map (Free $ vaps  reg) names
    Sqfm -> map (Free $ sqfms reg) names

send :: SynthRegister -> Parser [Action]
send reg = do
  word "free"
  synthDef <- synthDefName
  name <- anyWord
  case synthDef of
    Boop -> do msgs <- M.many $ parseBoopMsg
               return $ map (Send (boops reg) name) msgs
    Vap  -> do msgs <- M.many $ parseVapMsg
               return $ map  (Send (vaps  reg) name) msgs
    Sqfm -> do msgs <- M.many $ parseSqfmMsg
               return $ map (Send (sqfms reg) name) msgs
