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

-- | Per this, negative signs must abut the numbers they negate.
-- TODO: make it accept numbers without a decimal point, or with
-- no digits to the right of it.
  -- L.scientific doesn't accept those things either.
  -- pseudocode:
    -- parse the sign, or not, using option. return for that either 1 or -1.
    -- parse an int, or not, using option
    -- parse a period, or not, using option
    -- parse an int, or not, using option
    -- for each of those things that is missing, fill it with a default value
          -- note that this way xthe sign is never missing
    -- build a new number
    -- parse it using a built-in function like L.float or L.scientific
    -- multiply that by the sign
signedFloat :: Parser Float
signedFloat = L.signed nothing L.float where nothing = return ()
