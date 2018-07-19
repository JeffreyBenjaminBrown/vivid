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


wait :: Parser Action
wait = Wait <$> (word "wait" >> L.lexeme sc L.float)

synthDefName :: Parser SynthDefName
synthDefName = foldr1 (<|>) [ word "boop" >> return Boop
                            , word "vap" >> return Vap
                            , word "sqfm" >> return Sqfm
                            ]

new :: SynthRegister -> Parser Action
new reg = do
  word "new"
  synthDef <- synthDefName
  name <- anyWord
  case synthDef of
    Boop -> return $ New (boops reg) boop name
    Vap  -> return $ New (vaps  reg) vap  name
    Sqfm -> return $ New (sqfms reg) sqfm name

free :: SynthRegister -> Parser Action
free reg = do
  word "free"
  synthDef <- synthDefName
  name <- anyWord
  return $ case synthDef of
    Boop -> Free (boops reg) name
    Vap  -> Free (vaps  reg) name
    Sqfm -> Free (sqfms reg) name

send :: SynthRegister -> Parser [Action]
send reg = do
  word "free"
  synthDef <- synthDefName
  name <- anyWord
  case synthDef of
    Boop -> do msgs <- M.many $ parseBoopMsg
               return $ map (Send (boops reg) name) msgs
    Vap -> do msgs <- M.many $ parseVapMsg
              return $ map (Send (boops reg) name) msgs
    Sqfm -> do msgs <- M.many $ parseSqfmMsg
               return $ map (Send (boops reg) name) msgs
