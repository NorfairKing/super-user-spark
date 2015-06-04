{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.Test (htf_thisModulesTests) where


import           Parser
import           Test.Framework
import           Test.HUnit                    (Assertion)
import           Text.ParserCombinators.Parsec


testFileName = "testFileName.txt"

--[ Parser helper functions ]---

parserTest :: (Show a, Eq a) => SparkParser a -> a -> String -> Assertion
parserTest p result str = assertEqual (Right result) parseResult
  where parseResult = runParser p (initialState testFileName) testFileName str

parserTests :: (Show a, Eq a) => SparkParser a -> [(a, [String])] -> Assertion
parserTests p tests = sequence_ $ map (\(result, strs) -> sequence_ $ map (\s -> assertEqual (Right result) (parseResult s)) strs) tests
  where parseResult = runParser p (initialState testFileName) testFileName

parseSuccess :: SparkParser String -> String -> Assertion
parseSuccess p result = parserTest p result result

parseSuccesses :: SparkParser String -> [String] -> Assertion
parseSuccesses p results = sequence_ $ map (parseSuccess p) results


---[ Tests ]---

test_card_empty = parserTests card $
    [
        (Card Nothing [], [
              "card {}"
            , "card \n {}"
            , "\n\ncard \n\t\n{\n\t\r\n}\n"
            ]
        )
    ,   (Card (Just "") [], [
              "card \"\" {}"
            ]
        )
    ,   (Card (Just "hi") [], [
              "card hi {}"
            , "card \"hi\" {}"
            , "card \nhi\n{}"
            ]
        )
    ,   (Card (Just "something spaced") [], [
              "card \"something spaced\" {}"
            , "  card   \"something spaced\" {\n}"
            , " \t \n card \n\r  \"something spaced\" \t\n{\n\r}"
            ]
        )
    ]

test_cardContent = parserTests cardContent $
    [
        ([IntoDir "~", Deploy "bashrc" ".bashrc" UnspecifiedDeployment],
            [
              "{into ~;bashrc -> .bashrc}"
            , "{ into ~\n\tbashrc -> .bashrc}"
            , "{\n\tinto \"~\"\nbashrc -> .bashrc}"
            , "{\n    into ~\n    bashrc -> .bashrc\n}"
            , "{\n    into \"~\"\n    \"bashrc\" -> \".bashrc\"\n}"
            ]
        )
    ]

test_intoDir = parserTests intoDir $
    [
        (IntoDir "~", [
              "into ~"
            , "into \t  ~"
            , "into\t \t   ~"
            ]
        )
    ]

test_deployment = parserTests deployment $
    [
        (Deploy "bashrc" "/home/user/.bashrc" UnspecifiedDeployment, [
              "bashrc -> /home/user/.bashrc"
            , "bashrc \t->     /home/user/.bashrc"
            , "bashrc ->\"/home/user/.bashrc\""
            , "\"bashrc\"-> /home/user/.bashrc"
            , "\"bashrc\" -> \"/home/user/.bashrc\""
            , "\"bashrc\"->\"/home/user/.bashrc\""
            ]
        )
    ,   (Deploy "xmonad.hs" "/home/user/.xmonad/xmonad.hs" LinkDeployment, [
              "xmonad.hs l-> /home/user/.xmonad/xmonad.hs"
            , "\"xmonad.hs\"l-> /home/user/.xmonad/xmonad.hs"
            ]
        )
    ,   (Deploy "something with spaces" "/home/user/test.txt" CopyDeployment, [
              "\"something with spaces\"c->/home/user/test.txt"
            , "\"something with spaces\"\tc->/home/user/test.txt"
            ]
        )
    ]

test_deploymentKind_link    = parserTest deploymentKind LinkDeployment "l->"
test_deploymentKind_copy    = parserTest deploymentKind CopyDeployment "c->"
test_deploymentKind_default = parserTest deploymentKind UnspecifiedDeployment "->"

test_directory = parseSuccesses directory $
    [
        "~"
    ,   "~/"
    ,   "~/.vim"
    ,   "~/Dropbox"
    ]

test_filepath = parseSuccesses filepath $
    [
        "withoutExtension"
    ,   "test.txt"
    ,   "file.something"

    ,   "/home/user/test.txt"
    ,   "/home/user/test.txt"

    ]

test_filepath_quoted        = parserTest filepath "/home/user/long/path/with spaces" "\"/home/user/long/path/with spaces\""

-- TODO comments

test_inBraces_letter        = parserTest (inBraces word) "a" "{a}"
test_inBraces_word          = parserTest (inBraces word) "abc" "{abc}"

test_inQuotes_letter        = parserTest (inQuotes word) "a" "\"a\""
test_inQuotes_word          = parserTest (inQuotes word) "abc" "\"abc\""

test_inLineSpace = parserTests (inLineSpace word) $
    [
        ("a", [
                "   a \t "
                , " a "
                , "\ta\t"
                , "a"
              ]
        )
    ,   ("abc", [
                " abc "
                , "abc"
                , "abc\t\t\t\t"
                ]
        )
    ]

test_linespace_singleSpace  = parseSuccess linespace " "
test_linespace_many         = parseSuccess linespace " \t\t  "

test_witespace_space        = parseSuccess whitespace " "
test_witespace_tab          = parseSuccess whitespace "\t"
test_witespace_newline      = parseSuccess whitespace "\n"
test_witespace_return       = parseSuccess whitespace "\r"

test_whitespace_many        = parseSuccess whitespace "\n\r\t  \n\t\n   "
test_whitespace_spaces      = parseSuccess whitespace "    "

test_eol_windows            = parseSuccess eol "\n\r"
test_eol_backwardWindows    = parseSuccess eol "\r\n"
test_eol_linux              = parseSuccess eol "\n"
test_eol_max                = parseSuccess eol "\r"
