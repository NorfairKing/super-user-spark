module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either           (isLeft, isRight)

import           Text.Parsec
import           Text.Parsec.String

import           System.FilePath.Posix ((</>))

import           CoreTypes
import           Parser
import           Parser.Types

import           TestUtils

shouldSucceed :: (Show a, Eq a) => Parser a -> String -> IO ()
shouldSucceed parser input = input `shouldSatisfy` succeeds parser

shouldFail :: (Show a, Eq a) => Parser a -> String -> IO ()
shouldFail parser input = input `shouldNotSatisfy` succeeds parser

succeeds :: (Show a, Eq a) => Parser a -> String -> Bool
succeeds parser input = isRight $ parseWithoutSource (parser >> eof) input

fails :: (Show a, Eq a) => Parser a -> String -> Bool
fails parser input = not $ succeeds parser input

testInputSource :: String
testInputSource = "Test input"

parseShouldSucceedAs :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
parseShouldSucceedAs parser input a = parseFromSource parser testInputSource input `shouldBe` Right a

parseShouldBe :: (Show a, Eq a) => Parser a -> String -> Either ParseError a -> IO ()
parseShouldBe parser input result = parseFromSource parser testInputSource input `shouldBe` result


parseWithoutSource :: Parser a -> String -> Either ParseError a
parseWithoutSource parser input = parseFromSource parser testInputSource input

generateLetter :: Gen Char
generateLetter = elements $ ['a'..'z'] ++ ['A'..'Z']

generateWord :: Gen String
generateWord = listOf1 generateLetter

generateTab :: Gen Char
generateTab = return '\t'

generateSpace :: Gen Char
generateSpace = return ' '

generateLineFeed :: Gen Char
generateLineFeed= return '\n'

generateCarriageReturn :: Gen Char
generateCarriageReturn= return '\r'

generateLineSpace :: Gen String
generateLineSpace = listOf $ oneof [generateTab, generateSpace]

generateWhiteSpace :: Gen String
generateWhiteSpace = listOf $ oneof [generateTab, generateSpace, generateLineFeed, generateCarriageReturn]

generateWords :: Gen String
generateWords = fmap unwords $ listOf1 generateWord

generateEol :: Gen String
generateEol = elements ["\n", "\r", "\r\n"]

spec :: Spec
spec = do
    blankspaceParserTests
    enclosingCharacterTests
    delimiterTests
    identifierParserTests
    commentParserTests
    pathParserTests
    declarationParserTests
    parserBlackBoxTests


enclosingCharacterTests :: Spec
enclosingCharacterTests = do
    describe "inBraces" $ do
        it "succeeds for cases where we enclose braces around a string without braces" $ do
            forAll (listOf1 arbitrary `suchThat` (\c -> c /= "{" && c /= "}")) (\word ->
                parseShouldSucceedAs (inBraces $ string word) ("{" ++ word ++ "}") word)

    describe "inQuotes" $ do
        it "succeeds for cases where we enclose quotes around a string without quotes" $ do
            forAll (listOf1 arbitrary `suchThat` (/= "\"")) (\word ->
                parseShouldSucceedAs (inQuotes $ string word) ("\"" ++ word ++ "\"") word)

blankspaceParserTests :: Spec
blankspaceParserTests = do
    describe "eol" $ do
        let s = shouldSucceed eol
        it "succeeds for Linux line endings" $ do
            s "\n"
        it "succeeds for Windows line endings" $ do
            s "\r\n"
        it "succeeds for Mac line endings" $ do
            s "\r"
        let f = shouldFail eol
        it "fails for the empty string" $ do
            f ""
        it "fails for spaces" $ do
            property $ forAll (listOf generateSpace) (\ss -> f ss)
        it "fails for tabs" $ do
            property $ forAll (listOf generateTab) (\ss -> f ss)
        it "fails for linespace" $ do
            property $ forAll generateLineSpace (\ls -> f ls)

    describe "linespace" $ do
        let s = shouldSucceed whitespace
        it "succeeds for spaces" $ do
            property $ forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do
            property $ forAll (listOf generateTab) s
        it "succeeds for mixtures of spaces and tabs" $ do
            property $ forAll generateLineSpace s

        let f = shouldSucceed whitespace
        it "fails for line ending characters" $ do
            property $ forAll (listOf $ oneof [generateCarriageReturn, generateLineFeed]) f
        it "fails for any non-linespace, even if there's linespace in it" $  do
            property $ forAll (listOf1 generateLetter) (\ls ->
                        forAll generateLineSpace (\ls1 ->
                         forAll generateLineSpace (\ls2 ->
                            shouldFail linespace (ls1 ++ ls ++ ls2))))


    describe "whitespace" $ do
        let s = shouldSucceed whitespace
        it "succeeds for spaces" $ do
            property $ forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do
            property $ forAll (listOf generateTab) s
        it "succeeds carriage returns" $ do
            property $ forAll (listOf generateCarriageReturn) s
        it "succeeds line feeds" $ do
            property $ forAll (listOf generateLineFeed) s
        it "succeeds for mixtures of spaces, tabs, carriage returns and line feeds" $ do
            property $ forAll generateWhiteSpace s

        it "fails for any non-whitespace, even if there's whitespace in it" $  do
            property $ forAll (listOf1 generateLetter) (\ls ->
                        forAll generateWhiteSpace (\ws1 ->
                         forAll generateWhiteSpace (\ws2 ->
                            shouldFail whitespace (ws1 ++ ls ++ ws2))))


    describe "inLineSpace" $ do
        it "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            property $
                forAll generateLineSpace (\ws1 ->
                  forAll generateLineSpace (\ws2 ->
                    forAll (listOf1 generateLetter) (\ls ->
                        parseShouldSucceedAs (inLineSpace $ string ls) (ws1 ++ ls ++ ws2) ls)))

    describe "inWhiteSpace" $ do
        it "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            property $
                forAll generateWhiteSpace (\ws1 ->
                  forAll generateWhiteSpace (\ws2 ->
                    forAll (listOf1 generateLetter) (\ls ->
                        parseShouldSucceedAs (inWhiteSpace $ string ls) (ws1 ++ ls ++ ws2) ls)))

delimiterTests :: Spec
delimiterTests = do
    describe "delim" $ do
        it "succeeds on a semicolon" $ do
            shouldSucceed delim ";"
        it "succeeds on an eol" $ do
            once $ forAll (arbitrary `suchThat` succeeds eol) (shouldSucceed delim)

identifierParserTests :: Spec
identifierParserTests = do
    describe "plainIdentifier" $ do
        pend

    describe "quotedIdentifier" $ do
        pend


commentParserTests :: Spec
commentParserTests = do
    describe "eatComments" $ do
        pend

    describe "removeComments" $ do
        pend

    describe "notComment" $ do
        pend

    let makeLineComment = ("#" ++) . (++ "\n")
    describe "lineComment" $ do
        it "Succeeds for sentences starting with an \"#\" and ending in an end of line" $ do
            pending

    let makeBlockComment = ("[[" ++) . (++ "]]")
    describe "blockComment" $ do
        it "Succeeds for sentences starting with a \"[[\" and ending in \"]]\"" $ do
            pending

    describe "comment" $ do
        it "Succeeds for line comments" $ do
            once $ forAll (generateWords `suchThat` (succeeds lineComment . makeLineComment)) (shouldSucceed lineComment . makeLineComment)
        it "Succeeds for block comments" $ do
            once $ forAll (generateWords `suchThat` (succeeds blockComment . makeBlockComment)) (shouldSucceed blockComment . makeBlockComment)


pathParserTests :: Spec
pathParserTests = do
    describe "filepath" $ do
        pend

    describe "directory" $ do
        pend

twice gen = (,) <$> gen <*> gen

trice gen = (,,) <$> gen <*> gen <*> gen

generateCardName = generateWord

declarationParserTests :: Spec
declarationParserTests = do
    describe "card" $ do
        let pc = parseShouldSucceedAs card
        it "Succeeds on this card with an empty name correctly" $ do
            pc "card \"\" {}" $ Card "" testInputSource (Block [])

        it "Succeeds on this compressed empty cards" $ do
            forAll generateCardName $ \n ->
                parseShouldSucceedAs card ("card" ++ n ++ "{}") $ Card n testInputSource (Block [])

        it "Succeeds on empty cards with whitespace around the name" $ do
            forAll generateCardName $ \n ->
                forAll (twice generateWhiteSpace) $ \(ws1, ws2) ->
                    parseShouldSucceedAs card ("card" ++ ws1 ++ n ++ ws2 ++ "{}") $ Card n testInputSource (Block [])

        it "Succeeds on empty cards with whitespace between the brackets" $ do
            forAll generateCardName $ \n ->
                forAll generateWhiteSpace $ \ws ->
                    parseShouldSucceedAs card ("card" ++ n ++ "{" ++ ws ++ "}") $ Card n testInputSource (Block [])

        it "Fails on any card with an empty body" $ do
            forAll generateCardName $ \n ->
                forAll generateWhiteSpace $ \ws ->
                    shouldFail card ("card" ++ n ++ ws)

        it "Succeeds on this complicated example" $ do
            parseShouldSucceedAs card ("card complicated {\n  alternatives $(HOST) shared\n  hello l-> goodbye\n into $(HOME)\n  outof depot\n  spark card othercard\n  kind link\n  {\n    one c-> more\n    source -> destination\n    file\n  }\n}")
                $ Card "complicated" testInputSource $ Block
                    [
                      Alternatives ["$(HOST)", "shared"]
                    , Deploy "hello" "goodbye" (Just LinkDeployment)
                    , IntoDir "$(HOME)"
                    , OutofDir "depot"
                    , SparkOff (CardName (CardNameReference "othercard"))
                    , DeployKindOverride LinkDeployment
                    , Block [
                              Deploy "one" "more" (Just CopyDeployment)
                            , Deploy "source" "destination" Nothing
                            , Deploy "file" "file" Nothing
                            ]
                    ]

    describe "declarations" $ do
        pend

    describe "declaration" $ do
        pend

    describe "block" $ do
        pend

    describe "sparkOff" $ do
        pend

    describe "intoDir" $ do
        pend

    describe "outofDir" $ do
        pend

    describe "deployment" $ do
        pend

    describe "shortDeployment" $ do
        pend

    describe "longDeployment" $ do
        pend

    describe "deploymentKind" $ do
        pend

    describe "alternatives" $ do
        pend

toplevelParserTests :: Spec
toplevelParserTests = do
    describe "sparkFile" $ do
        pend

    describe "resetPosition" $ do
        pend

    describe "getFile" $ do
        pend

cardReferenceParserTests :: Spec
cardReferenceParserTests = do
    describe "compilerCardReference" $ do
        pend

    describe "deployerCardReference" $ do
        pend

    describe "compiledCardReference" $ do
        pend

    describe "cardReference" $ do
        pend

    describe "cardNameReference" $ do
        pend

    describe "cardNameReference" $ do
        pend

    describe "cardName" $ do
        pend

    describe "cardFileReference" $ do
        pend

    describe "unprefixedCardFileReference" $ do
        pend



parserBlackBoxTests :: Spec
parserBlackBoxTests = do
    let tr = "test_resources"
    describe "Correct succesful parse examples" $ do
        let dirs = map (tr </>) ["shouldParse", "shouldCompile", "shouldNotCompile"]
        forFileInDirss dirs $ concerningContents $ \f contents -> do
            it f $ parseFromSource sparkFile f contents `shouldSatisfy` isRight

    describe "Correct unsuccesfull parse examples" $ do
        let dirs = map (tr </>) ["shouldNotParse"]
        forFileInDirss dirs $ concerningContents $ \f contents -> do
            it f $ parseFromSource sparkFile f contents `shouldSatisfy` isLeft




