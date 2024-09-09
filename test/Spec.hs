{-# LANGUAGE BlockArguments #-}
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, assertFailure)
import Parser (runClassParser)

generateClassTest :: String -> Assertion
generateClassTest x = do 
  let javaFilePath =  "./test/CodeExamples/"<>x<>".java"
  javaCode <- readFile javaFilePath
  let expectedFilePath = "./test/CodeExamples/"<>x<>".expected"
  shouldBe <- read <$> readFile expectedFilePath
  case runClassParser javaCode of
    Left err -> assertFailure $ "Parse error" <> show err
    Right r -> r @?= shouldBe


main :: IO ()
main = defaultMain $
  testGroup "Class Tests" [
    testCase "Basic" $ 
      generateClassTest "Car"
  ]

