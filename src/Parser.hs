{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser (
    runClassParser,
) where

import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad
import Data.Bifunctor (first,second, Bifunctor (second))
import Data.Void (Void)

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

findNextInstance :: Parser a -> Parser a
findNextInstance end = go
    where
        go = end <|> (anySingle >> go)

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
    optional $ try $ string "static" <* space1
    nextKw <- stringUntil (lookAhead $ stringOptions [" ","("]) <* space
    r <- optional $ lookAhead $ string "("
    case r of
        Just _ -> do
            MethodSignature access "Constructor" nextKw <$> methodParams
        Nothing -> do
            name <- stringUntil (lookAhead $ stringOptions [" ", "("]) <* space
            MethodSignature access nextKw name <$> methodParams


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

-- The many combinator extended to having two optional parsers
many2 :: Parser a -> Parser b -> Parser ([a],[b])
many2 p1 p2 = go
    where
        go = do
            t1 <- optional (try p1)
            case t1 of
                (Just r) ->  first (r :) <$> go
                Nothing -> do
                    t2 <- optional (try p2)
                    case t2 of
                        (Just r) -> second (r :) <$> go
                        Nothing -> pure ([],[])

-- Parser for a class 
classParser :: Parser Class
classParser = lexeme $ do
    spacesAndComments
    classSig <- findNextInstance classSignature
    (m,a) <- many2 method attributeSignature
    space >> string "}"
    pure $ Class classSig m a


runClassParser :: String -> Either (ParseErrorBundle String Void) Class
runClassParser = parse (classParser <* eof) ""
