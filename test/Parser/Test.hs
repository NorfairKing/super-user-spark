{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.Test (htf_thisModulesTests) where

import           Control.Applicative
import           Test.Framework
import           Test.HUnit                    (Assertion)
import           Text.ParserCombinators.Parsec

import           Parser
import           Types


testFileName = "parser testinput"

--[ Parser helper functions ]---

parserTest :: (Show a, Eq a) => Parser a -> a -> String -> Assertion
parserTest p result str = assertEqual (Right result) parseResult
  where parseResult = parse p testFileName str

parserTests :: (Show a, Eq a) => Parser a -> [(a, [String])] -> Assertion
parserTests p tests = sequence_ $ map (\(result, strs) -> sequence_ $ map (\s -> assertEqual (Right result) (parseResult s)) strs) tests
  where parseResult = parse p testFileName

parseItself :: Parser String -> String -> Assertion
parseItself p str = parserTest p str str

parseSuccess :: Parser String -> String -> Assertion
parseSuccess p str = (assertRight $ parse (p <* eof) testFileName str) >> return ()

parseFail :: Parser String -> String -> Assertion
parseFail p str = (assertLeft $ parse (p <* eof) testFileName str) >> return ()

parseItselfs :: Parser String -> [String] -> Assertion
parseItselfs p strs = sequence_ $ map (parseItself p) strs

parseSuccesses :: Parser String -> [String] -> Assertion
parseSuccesses p strs = sequence_ $ map (parseSuccess p) strs

parseFails :: Parser String -> [String] -> Assertion
parseFails p strs = sequence_ $ map (parseFail p) strs




--[ Language ]--
test_Block = parserTests block $
    [
        (Block [IntoDir "~", Deploy "bashrc" ".bashrc" Nothing],
            [
              "{into ~;bashrc -> .bashrc}"
            , "{into ~ ; \tbashrc -> .bashrc;}"
            , "{ into ~\n\tbashrc -> .bashrc}"
            , "{\n\tinto \"~\"\nbashrc -> .bashrc}"
            , "{\n    into ~\n    bashrc -> .bashrc\n}"
            , "{\n    into \"~\"\n    \"bashrc\" -> \".bashrc\"\n}"
            ]
        )
    ]

test_sparkOff = parserTests sparkOff $
    [
        (SparkOff (CardName $ CardNameReference "name"),
            [
              "spark card name"
            , "sparkcard \"name\""
            , "spark\tcard\tname"
            , "spark \tcard\t \tname"
            ]
        )
    ]

test_cardNameReference = parserTests cardNameReference $
    [
        (CardNameReference "name",
            [
              "card name"
            , "card \"name\""
            , "card\tname"
            , "card\t \tname"
            ]
        )
    ]

test_cardFileReference = parserTests cardFileReference $
    [
        (CardFileReference "card.sus" Nothing,
            [
              "file card.sus"
            , "file \"card.sus\""
            , "file\tcard.sus"
            , "file \t card.sus"
            ]
        )
    ,   (CardFileReference "card.sus" (Just $ CardNameReference "name"),
            [
              "file card.sus name"
            , "file \"card.sus\" \"name\""
            , "file\tcard.sus\tname"
            , "file \t card.sus \t name"
            ]
        )
    ]

test_intoDir = parserTests intoDir $
    [
        (IntoDir "~", [
              "into ~"
            , "into \t  ~"
            , "into\t \t   ~"
            , "into \"~\""
            ]
        )
    ,   (IntoDir "~/.xmonad", [
              "into ~/.xmonad"
            , "into \"~/.xmonad\""
            , "into ~/.xmonad/"
            ]
        )
    ]

test_outofDir = parserTests outOfDir $
    [
        (OutofDir "bash", [
              "outof bash"
            , "outof \t bash"
            , "outof \"bash\""
            , "outof        bash"
            ]
        )
    ,   (OutofDir "xmonad", [
              "outof xmonad"
            , "outof \t\t\txmonad"
            , "outof \"xmonad\""
            , "outof      \txmonad"
            ]
        )
    ]

test_deployment = parserTests deployment $
    [
        (Deploy "bashrc" "/home/user/.bashrc" Nothing, [
              "bashrc -> /home/user/.bashrc"
            , "bashrc \t->     /home/user/.bashrc"
            , "bashrc ->\"/home/user/.bashrc\""
            , "\"bashrc\"-> /home/user/.bashrc"
            , "\"bashrc\" -> \"/home/user/.bashrc\""
            , "\"bashrc\"->\"/home/user/.bashrc\""
            ]
        )
    ,   (Deploy "xmonad.hs" "/home/user/.xmonad/xmonad.hs" (Just LinkDeployment), [
              "xmonad.hs l-> /home/user/.xmonad/xmonad.hs"
            , "\"xmonad.hs\"l-> /home/user/.xmonad/xmonad.hs"
            ]
        )
    ,   (Deploy "something with spaces" "/home/user/test.txt" (Just CopyDeployment), [
              "\"something with spaces\"c->/home/user/test.txt"
            , "\"something with spaces\"\tc->/home/user/test.txt"
            ]
        )
    ,   (Deploy "file.txt" "file.txt" Nothing, [
              "file.txt"
            , "\"file.txt\""
            ]
        )
    ]
