{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Parser.Test (htf_thisModulesTests) where

import           Test.Framework
import           Test.HUnit                    (Assertion)
import           Text.ParserCombinators.Parsec

import           Parser
import           Types


testFileName = "testFileName.txt"

--[ Parser helper functions ]---

parserTest :: (Show a, Eq a) => Parser a -> a -> String -> Assertion
parserTest p result str = assertEqual (Right result) parseResult
  where parseResult = parse p testFileName str

parserTests :: (Show a, Eq a) => Parser a -> [(a, [String])] -> Assertion
parserTests p tests = sequence_ $ map (\(result, strs) -> sequence_ $ map (\s -> assertEqual (Right result) (parseResult s)) strs) tests
  where parseResult = parse p testFileName

parseSuccess :: Parser String -> String -> Assertion
parseSuccess p result = parserTest p result result

parseSuccesses :: Parser String -> [String] -> Assertion
parseSuccesses p results = sequence_ $ map (parseSuccess p) results


test_gitRepo = parserTests gitRepo $
    [
        (GitRepo {repo_protocol = HTTPS, repo_host = "github.com", repo_path = "NorfairKing/sus-depot"},
        ["https://github.com/NorfairKing/sus-depot"])
    ,   (GitRepo {repo_protocol = Git  , repo_host = "github.com", repo_path = "NorfairKing/sus-depot"},
        ["git@github.com:NorfairKing/sus-depot.git"])
    ,   (GitRepo {repo_protocol = HTTPS, repo_host = "bitbucket.org", repo_path = "syd_kerckhove/private-depot"},
        ["https://bitbucket.org/syd_kerckhove/private-depot"])
    ,   (GitRepo {repo_protocol = Git  , repo_host = "bitbucket.org", repo_path = "syd_kerckhove/private-depot"},
        ["git@bitbucket.org:syd_kerckhove/private-depot.git"])
    ]


---[ Tests ]---

test_card_empty = parserTests card $
    [
    (Card "" testFileName[], [
              "card \"\" {}"
            ]
        )
    ,   (Card "hi" testFileName [], [
              "card hi {}"
            , "card \"hi\" {}"
            , "card \nhi\n{}"
            ]
        )
    ,   (Card "something spaced" testFileName [], [
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

test_lineComment = error "not implemented"
test_lineComment = error "not implemented"

test_inBraces_letter        = parserTest (inBraces plainIdentifier) "a" "{a}"
test_inBraces_word          = parserTest (inBraces plainIdentifier) "abc" "{abc}"

test_inQuotes_letter        = parserTest (inQuotes plainIdentifier) "a" "\"a\""
test_inQuotes_word          = parserTest (inQuotes plainIdentifier) "abc" "\"abc\""

test_delim = parseSuccesses delim $
    [
        ";"
    ,   "\n"
    ,   "\r"
    ,   "\n\r"
    ,   "\r\n"
    ,   "\n\r  \t \n\t \n"
    ]


test_inLineSpace = parserTests (inLineSpace plainIdentifier) $
    [
        ("a", [
                "a"
                , "   a \t "
                , " a "
                , "\ta\t"
              ]
        )
    ,   ("abc", [
                "abc"
                , " abc "
                , "abc"
                , "abc\t\t\t\t"
                ]
        )
    ]

test_inWhiteSpace = parserTests (inWhiteSpace plainIdentifier) $
    [
        ("a", [
                "a"
                , " \n\r  a \t "
                , " a\n "
                , "\ta\r\t"
              ]
        )
    ,   ("abc", [
                "abc"
                , " abc "
                , "abc\t\t\t\t"
                ]
        )
    ]

test_linespace  = parseSuccesses linespace $
    [
        ""
    ,   " "
    ,   "\t"
    ,   " \t"
    ,   "\t "
    ,   "\t  \t\t\t  \t\t \t"
    ]

test_whitespace = parseSuccesses whitespace $
    [
        ""
    ,   " "
    ,   "\t"
    ,   "\n"
    ,   "\r"
    ,   " \t"
    ,   "\n\r"
    ,   " \t\n\r"
    ,   " \t \n \r\n\t\t\t  \n\n\r\n"
    ]

test_eol = parseSuccesses eol $
    [
        "\n\r"
    ,   "\r\n"
    ,   "\n"
    ,   "\r"
    ]

