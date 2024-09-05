{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser (
    runClassParser
) where

import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad


classAccessModifierMappings :: [(String, ClassAccessModifier)]
classAccessModifierMappings = [("public",PublicClass),("private",PrivateClass)]

accessModifierMappings :: [(String, AccessModifier)]
accessModifierMappings = [("public",Public),("private",Private),("protected",Protected)]

-- Parses whitespace and comments, returning nothing
spacesAndComments :: Parser ()
spacesAndComments = L.space space1 (L.skipLineComment "//") empty

-- Takes in a parser and returns that parser that also does spaces and comments afterwards
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spacesAndComments

-- Tries to parse each string in a list in turn
stringOptions :: [String] -> Parser String
stringOptions = choice . map (try . string)

-- Gets a string until a specified parser succeeds
stringUntil :: Parser a -> Parser String
stringUntil p = someTill anySingle (try p)

-- Parses from a list of strings, returning what that string is mapped to
stringValMap :: [(String,a)] -> Parser a
stringValMap =  choice . map (\(s,t) -> t <$ try (string s))

-- Clear until a string appears
clearUntil :: Parser a -> Parser ()
clearUntil p = Control.Monad.void (manyTill anySingle p)

clearUntilString :: String -> Parser ()
clearUntilString s = clearUntil (try $ string s)

-- Requires a keyword in a signature, allowing arbitrary whitespace
requireKeyword :: String -> Parser String
requireKeyword s = string s <* space1

--Parses a standard access modifier
accessModifier :: Parser AccessModifier
accessModifier = stringValMap accessModifierMappings <* space1

-- Parses a class signature
classSignature :: Parser ClassSignature
classSignature = lexeme $ do
    access <- stringValMap classAccessModifierMappings <* space1
    requireKeyword "class"
    name <- stringUntil (stringOptions ["{"," "]) <* space
    optional $ string "{"
    pure $ ClassSignature access name

-- Parses an attribute signature
attributeSignature :: Parser AttributeSignature
attributeSignature = lexeme $ do
    access <- accessModifier
    attType <- stringUntil (string " ") <* space
    name <- stringUntil $ lookAhead $ stringOptions [" ","=",";"]
    clearUntilString ";"
    pure $ AttributeSignature access attType name


--Parses a type key pair in a method signature 
methodParameter :: Parser (String,String)
methodParameter = lexeme $ do
    space
    paramType <- stringUntil (string " ") <* space
    paramName <- stringUntil $ lookAhead $ stringOptions [" ",",",")"]
    space
    optional $ string ","
    pure (paramType,paramName)

--Parses the list of method parameters from a method signature
methodParams :: Parser [(String,String)]
methodParams = string "(" >> manyTill methodParameter (try $ string ")")

-- Parses a method signature
methodSignature :: Parser MethodSignature
methodSignature = lexeme $ do
    access <- accessModifier
    attType <- stringUntil (string " ") <* space
    name <- stringUntil (lookAhead $ stringOptions [" ", "("]) <* space
    MethodSignature access attType name <$> methodParams

-- This parser will find the next '{', and then consume the entire scope that it opens.
scopeConsumer :: Parser ()
scopeConsumer = lexeme $ scopeConsumer' (-1)

scopeConsumer' :: Int -> Parser ()
scopeConsumer' x = do
    clearUntil $ lookAhead $ stringOptions ["{","}"]
    s <- anySingle
    case s of
        '{' -> scopeConsumer' (x+1)
        '}' -> if x==0 then pure () else scopeConsumer' (x-1)
        _ -> pure ()

-- Parser for a method, returning the method signature
method :: Parser MethodSignature
method = methodSignature <* (space >> scopeConsumer)


runClassParser :: String -> IO ()
runClassParser s = case parse (methodSignature <* eof) "" s of
    Left err  -> putStrLn (errorBundlePretty err)
    Right res -> print res
