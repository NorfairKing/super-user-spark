{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.Parser.TestUtils where

import TestImport hiding (succeeds)

import Data.Either (isRight)
import SuperUserSpark.Parser.Internal
import Text.Parsec
import Text.Parsec.String

shouldSucceed
    :: (Show a, Eq a)
    => Parser a -> String -> IO ()
shouldSucceed parser input = input `shouldSatisfy` succeeds parser

shouldFail
    :: (Show a, Eq a)
    => Parser a -> String -> IO ()
shouldFail parser input = input `shouldNotSatisfy` succeeds parser

succeeds :: Parser a -> String -> Bool
succeeds parser = succeedsWithLeftover $ parser >> eof

succeedsWithLeftover :: Parser a -> String -> Bool
succeedsWithLeftover parser input = isRight $ parseWithoutSource parser input

succeedsAnywhere :: Parser a -> String -> Bool
succeedsAnywhere p s = or $ map (succeedsWithLeftover p) (tails s)
  where
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails ass@(_:as) = ass : tails as

fails :: Parser a -> String -> Bool
fails parser input = not $ succeeds parser input

testInputSource :: Path Abs File
testInputSource = $(mkAbsFile "/Test/input/file")

parseShouldSucceedAs
    :: (Show a, Eq a)
    => Parser a -> String -> a -> IO ()
parseShouldSucceedAs parser input a =
    parseFromSource parser testInputSource input `shouldBe` Right a

parseShouldBe
    :: (Show a, Eq a)
    => Parser a -> String -> Either ParseError a -> IO ()
parseShouldBe parser input result =
    parseFromSource parser testInputSource input `shouldBe` result

parseWithoutSource :: Parser a -> String -> Either ParseError a
parseWithoutSource parser input = parseFromSource parser testInputSource input
