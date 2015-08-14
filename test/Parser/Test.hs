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




---[ Tests ]---

--[ Dispatch ]--

test_parseParse = parserTests parseParse $
    [
    (DispatchParse "test.sus",
            [
                "parsetest.sus"
            ,   "parse test.sus"
            ,   "parse   test.sus"
            ,   "parse\ttest.sus"
            ,   "parse\t \t test.sus"
            ]
        )
    ]

test_parseFormat = parserTests parseFormat $
    [
    (DispatchFormat "test.sus",
            [
                "formattest.sus"
            ,   "format test.sus"
            ,   "format   test.sus"
            ,   "format\ttest.sus"
            ,   "format\t \t test.sus"
            ]
        )
    ]

test_parseCompile = parserTests parseCompile $
    [
        (DispatchCompile (CardFileReference "test.sus" Nothing),
            [
                "compiletest.sus"
            ,   "compile test.sus"
            ,   "compile   test.sus"
            ,   "compile\ttest.sus"
            ,   "compile\t \t test.sus"
            ]
        )
    ,   (DispatchCompile (CardFileReference "test.sus" $ Just $ CardNameReference "card"),
            [
                "compiletest.sus card"
            ,   "compile test.sus card"
            ,   "compile   test.sus card"
            ,   "compile\ttest.sus card"
            ,   "compile\t \t test.sus card"
            ]
        )
    ]

test_parseCheck = parserTests parseCheck $
    [
        (DispatchCheck (CheckerCardCompiled "test.cd"),
            [
                "check compiled test.cd"
            ,   "checkcompiled test.cd"
            ,   "check compiledtest.cd"
            ,   "checkcompiledtest.cd"
            ]
        )
    ,   (DispatchCheck (CheckerCardUncompiled $ CardFileReference "test.sus" Nothing),
            [
                "check test.sus"
            ,   "checktest.sus"
            ]
        )
    ]

test_parseDeploy = parserTests parseDeploy $
    [
        (DispatchDeploy (DeployerCardCompiled "test.cd"),
            [
                "compiledtest.cd"
            ,   "compiled test.cd"
            ]
        )
    ,   (DispatchDeploy (DeployerCardUncompiled $ StartFile $ CardFileReference "test.sus" Nothing),
            [
                "filetest.sus"
            ,   "file test.sus"
            ]
        )
    ,   (DispatchDeploy (DeployerCardUncompiled $ StartFile $ CardFileReference "test.sus" $ Just $ CardNameReference "card"),
            [
                "filetest.sus card"
            ,   "file test.sus card"
            ]
        )
    ]
-- TODO tests with StartRepo"


--[ Language ]--

test_card_empty = parserTests card $
    [
    (Card "" testFileName [], [
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

test_cardRepoReference = parserTests cardRepoReference $
    [
        (CardRepoReference repo Nothing Nothing,
            [
              "git " ++ repoStr
            , "git\t" ++ repoStr
            , "git \t" ++ repoStr
            , "git        " ++ repoStr
            ]
        )
    ,   (CardRepoReference repo (Just "development") Nothing,
            [
              "git " ++ repoStr ++ ":development"
            , "git\t" ++ repoStr ++ ":development"
            , "git \t" ++ repoStr ++ ":development"
            , "git        " ++ repoStr ++ ":development"
            ]
        )
    ,   (CardRepoReference repo Nothing (Just $ CardFileReference "card.sus" Nothing),
            [
              "git " ++ repoStr ++ " card.sus"
            , "git\t\t" ++ repoStr ++ "\t\tcard.sus"
            , "git " ++ repoStr ++ " \"card.sus\""
            ]
        )
    ,   (CardRepoReference repo Nothing (Just $ CardFileReference "card.sus" $ Just $ CardNameReference "name"),
            [
              "git " ++ repoStr ++ " card.sus name"
            , "git\t\t" ++ repoStr ++ "\t\tcard.sus\tname"
            , "git " ++ repoStr ++ " \"card.sus\" \"name\""
            ]
        )
    ,   (CardRepoReference repo (Just "master") (Just $ CardFileReference "card.sus" $ Just $ CardNameReference "name"),
            [
              "git " ++ repoStr ++ ":master card.sus name"
            , "git\t\t" ++ repoStr ++ ":master\t\tcard.sus\tname"
            , "git " ++ repoStr ++ ":master \"card.sus\" \"name\""
            ]
        )
    ]
  where
    repo = GitRepo {repo_protocol = Git  , repo_host = "bitbucket.org", repo_path = "syd_kerckhove/private-depot"}
    repoStr = "git@bitbucket.org:syd_kerckhove/private-depot.git"

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

test_deploymentKind_link    = parserTest deploymentKind (Just LinkDeployment) "l->"
test_deploymentKind_copy    = parserTest deploymentKind (Just CopyDeployment) "c->"
test_deploymentKind_default = parserTest deploymentKind Nothing "->"

test_directory = parseItselfs directory $
    [
        "~"
    ,   "~/.vim"
    ,   "~/Dropbox"

    ,   "/home/user"
    ,   "/home/user/.xmonad"
    ]

test_filepath = parseItselfs filepath $
    [
        "withoutExtension"
    ,   "test.txt"
    ,   "file.somelongextension"

    ,   "/home/user/test.txt"
    ,   "/home/user/test.multiple.extensions"

    ,   "/home/user/../user/test.txt"
    ]

test_filepath_quoted = parserTest filepath "/home/user/long/path/with spaces" "\"/home/user/long/path/with spaces\""

test_lineComment = parseSuccesses lineComment $
    [
        "#hello\n", "#hello\n\r"
    ,   "# This is a very long\tline comment\t with whitespaces \n"
    ]

test_blockComment = parseSuccesses blockComment $
    [
        "[[ hellokidoki ]]"
    ,   "[[ This is a very long block comment\n with \n\r whitespace\n ]]"
    ]


--[ Identifiers ]--

test_plainIdentifier_success  = parseItselfs plainIdentifier $
    [
        "test"
    ,   "thing"
    ,   "sus-depot"
    ,   "super_user_spark"
    ,   "super.user.spark"
    ,   "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
    ]

test_plainIdentifier_fail     = parseFails plainIdentifier $
    [
        "\"identifier\""
    ,   "\"", "\n", "\r", ";", " ", "\t", "{", "}"
    ,   "sus depot"
    ]

test_quotedIdentifier_success = parseSuccesses quotedIdentifier $
    [
        "\"a\"", "\"abc\"", "\"abcdefghijklmnopqrstuvwxyz\""
    ,   "\" \"", "\"\t\"", "\";\"", "\"{\"", "\"}\""
    ,   "\"sus depot\"", "\"sus\tdepot\""
    ]

test_quotedIdentifier_fail    = parseFails quotedIdentifier $
    [
        "\""
    ,   "\n", "\r", ";", " ", "\t", "{", "}"
    ,   "\"\n\"", "\"\r\"", " ", "\t", "{", "}"
    ,   "\"a", "\"abc"
    ,   "a\"", "abc\""
    ]



--[ Delimiters ]--

test_inBraces_letter        = parserTest (inBraces plainIdentifier) "a" "{a}"
test_inBraces_word          = parserTest (inBraces plainIdentifier) "abc" "{abc}"

test_inQuotes_letter        = parserTest (inQuotes plainIdentifier) "a" "\"a\""
test_inQuotes_word          = parserTest (inQuotes plainIdentifier) "abc" "\"abc\""


test_delim = parseItselfs delim $
    [
        ";"
    ,   "\n"
    ,   "\r"
    ,   "\n\r"
    ,   "\r\n"
    ,   "\n\r  \t \n\t \n"
    ]


--[ Whitespace ]--

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

test_linespace  = parseItselfs linespace $
    [
        ""
    ,   " "
    ,   "\t"
    ,   " \t"
    ,   "\t "
    ,   "\t  \t\t\t  \t\t \t"
    ]

test_whitespace = parseItselfs whitespace $
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

test_eol = parseItselfs eol $
    [
        "\n\r"
    ,   "\r\n"
    ,   "\n"
    ,   "\r"
    ]

test_eol_fail = parseFails eol $
    [
        ""
    ,   " "
    ,   "\t"
    ,   "\t "
    ,   " \t"
    ]

--[ Git ]--

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

test_gitProtocol = parserTests gitProtocol $
    [
        (HTTPS, ["https://"])
    ,   (Git, ["git@"])
    ]
