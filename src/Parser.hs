{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parser where 

import Types ( Parser )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void


spacesAndComments :: Parser ()
spacesAndComments = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spacesAndComments

pubpriv :: Parser () 





