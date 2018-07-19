module Vivid.Jbb.Parse where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib
import Vivid.Jbb.ParseUtils


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

--freq :: Parser (Msg superset)
--freq = do word "freq" >> 

--send :: SynthRegister -> Parser Action
--send reg = do
--  word "free"
--  synthDef <- synthDefName
--  name <- anyWord
--  msg <- _
--  case synthDef of
--    Boop -> return $ Send (boops reg) name msg
--    Vap  -> return $ Send (vaps  reg) name msg
--    Sqfm -> return $ Send (sqfms reg) name msg
