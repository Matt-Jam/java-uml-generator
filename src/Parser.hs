{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parser where 

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

spacesAndComments :: Parser ()
spacesAndComments = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spacesAndComments


