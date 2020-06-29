{-# LANGUAGE DataKinds #-}

module Montevideo.Dispatch.Parse where

import Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (sepBy1)

import Vivid
import Dispatch.Types
import Dispatch.Act
import Dispatch.Parse.Params
import Dispatch.Parse.Utils
import Synths


synthDefName :: Parser SynthDefEnum
synthDefName = foldr1 (<|>) [ word "boop" >> return Boop
                            , word "vap" >> return Vap
                            , word "sqfm" >> return Sqfm
                            ]

msgs :: SynthRegister -> Parser [Action']
msgs reg = concat <$>
  sepBy1 (homogeneousMsgs reg) (L.lexeme sc $ C.string ",")

-- | msgs all of the same type, e.g. a bunch of News, or a bunch of Frees
homogeneousMsgs :: SynthRegister -> Parser [Action']
homogeneousMsgs reg = L.lexeme sc $ foldl1 (<|>)
  [ parseNews reg, parseFrees reg, parseSends reg ]

-- everything below includes per-synth boilerplate

parseNews :: SynthRegister -> Parser [Action']
parseNews reg = do
  word "new"
  synthDef <- synthDefName
  names <- M.many anyWord
  case synthDef of
    Boop -> return $ map (New' (boops reg) boop) names
    Vap  -> return $ map (New' (vaps  reg) vap ) names
    Sqfm -> return $ map (New' (sqfms reg) sqfm) names

parseFrees :: SynthRegister -> Parser [Action']
parseFrees reg = do
  word "free"
  synthDef <- synthDefName
  names <- M.many $ anyWord
  return $ case synthDef of
    Boop -> map (Free' $ boops reg) names
    Vap  -> map (Free' $ vaps  reg) names
    Sqfm -> map (Free' $ sqfms reg) names

parseSends :: SynthRegister -> Parser [Action']
parseSends reg = do
  word "send"
  synthDef <- synthDefName
  name <- anyWord
  case synthDef of
    Boop -> do msgs <- M.many $ parseBoopMsg
               return $ map (Send' (boops reg) name) msgs
    Vap  -> do msgs <- M.many $ parseVapMsg
               return $ map  (Send' (vaps  reg) name) msgs
    Sqfm -> do msgs <- M.many $ parseSqfmMsg
               return $ map (Send' (sqfms reg) name) msgs
