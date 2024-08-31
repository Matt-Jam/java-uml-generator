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
}


data MethodSignature = MethodSignature {
    methodAccessModifier :: AccessModifier,
    methodName :: String,
    methodReturnType :: String,
    methodParameters :: [String]
}


data AttributeSignature = AttributeSignature {
    attributeAccessModifier :: AccessModifier,
    attributeName :: String,
    atrributeType :: String
}


data Class = Class {
    signature :: ClassSignature,
    methods :: [MethodSignature],
    attributes :: [AttributeSignature]
}