module Types where 

import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String


data ClassAccessModifier = PublicClass | PrivateClass
    deriving(Eq,Show)


data AccessModifier = Public | Private | Protected 
    deriving(Eq,Show)


data ClassSignature = ClassSignature {
    classAccessModifier :: ClassAccessModifier,
    className :: String
} deriving(Eq,Show)


data MethodSignature = MethodSignature {
    methodAccessModifier :: AccessModifier,
    methodReturnType :: String,
    methodName :: String,
    methodParameters :: [(String, String)]
} deriving (Eq,Show)

data AttributeSignature = AttributeSignature {
    attributeAccessModifier :: AccessModifier,
    atrributeType :: String,
    attributeName :: String
} deriving (Eq,Show)


data Class = Class {
    signature :: ClassSignature,
    methods :: [MethodSignature],
    attributes :: [AttributeSignature]
}