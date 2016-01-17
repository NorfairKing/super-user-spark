module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either        (isRight)

import           Text.Parsec
import           Text.Parsec.String

import           Parser
import           Parser.Types

parseSucceeds :: (Show a, Eq a) => Parser a -> String -> IO ()
parseSucceeds parser input = parseWithoutSource parser input `shouldSatisfy` isRight

parseShouldBe :: (Show a, Eq a) => Parser a -> String -> Either ParseError a -> IO ()
parseShouldBe parser input result = parseWithoutSource parser input `shouldBe` result

parseWithoutSource :: Parser a -> String -> Either ParseError a
parseWithoutSource parser input = parseFromSource parser "Test input" input

spec :: Spec
spec = do
    describe "eol" $ do
        let t = parseSucceeds eol
        it "succeeds for Linux line endings" $ do
            t "\n"
        it "succeeds for Windows line endings" $ do
            t "\r\n"
        it "succeeds for Mac line endings" $ do
            t "\r"

    describe "linespace" $ do
        let t = parseSucceeds whitespace
            m c = property $ (\i -> t $ replicate i c)
        it "succeeds for spaces" $ do
            m ' '
        it "succeeds for tabs" $ do
            m '\t'
        it "succeeds for mixtures of spaces and tabs" $ do
            t " \t"
            t "\t "
            t "\t  \t\t\t  \t\t \t"



    describe "whitespace" $ do
        let t = parseSucceeds whitespace
            m c = property $ (\i -> t $ replicate i c)
        it "succeeds for spaces" $ do
            m ' '
        it "succeeds for tabs" $ do
            m '\t'
        it "succeeds carriage returns" $ do
            m '\r'
        it "succeeds line feeds" $ do
            m '\n'
        it "succeeds for mixtures of spaces, tabs, carriage returns and line feeds" $ do
            t "\n\r"
            t " \t\n\r"
            t " \t \n \r\n\t\t\t  \n\n\r\n"




