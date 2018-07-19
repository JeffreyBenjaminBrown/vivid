module Vivid.Jbb.ParseUtils where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space C.space1 empty empty

parens :: Parser a -> Parser a
parens = between (L.symbol sc "(") (L.symbol sc ")")

brackets :: Parser a -> Parser a
brackets = between (L.symbol sc "[") (L.symbol sc "]")

wordChar :: Parser Char
wordChar = C.alphaNumChar <|> C.char '_' <|> C.char '\'' 

-- | Succeeds only if the next character is not a word character. Examples:
-- M.parse (word "monk") "" "monkey" -- fails
-- M.parse (word "monk") "" "monk eye" -- works
-- M.parse (word "monk") "" "monk. eye" -- works
word :: String -> Parser String
word w = try $ L.lexeme sc $ C.string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = L.lexeme sc $ some wordChar  <* notFollowedBy wordChar
