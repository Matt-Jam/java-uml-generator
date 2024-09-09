module Types (
    Parser,
    ClassAccessModifier(..),
    AccessModifier(..),
    ClassSignature(..),
    MethodSignature(..),
    AttributeSignature(..),
    Class(..)
) where 

import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String


data ClassAccessModifier = PublicClass | PrivateClass
    deriving(Eq,Show, Read)


data AccessModifier = Public | Private | Protected 
    deriving(Eq,Show, Read)


data ClassSignature = ClassSignature {
    classAccessModifier :: ClassAccessModifier,
    className :: String
} deriving(Eq,Show, Read)


data MethodSignature = MethodSignature {
    methodAccessModifier :: AccessModifier,
    methodReturnType :: String,
    methodName :: String,
    methodParameters :: [(String, String)]
} deriving (Eq,Show, Read)

data AttributeSignature = AttributeSignature {
    attributeAccessModifier :: AccessModifier,
    atrributeType :: String,
    attributeName :: String
} deriving (Eq,Show, Read)


data Class = Class {
    signature :: ClassSignature,
    methods :: [MethodSignature],
    attributes :: [AttributeSignature]
} deriving (Eq, Show, Read)