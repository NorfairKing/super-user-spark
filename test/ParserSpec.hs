module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either           (isLeft, isRight)

import           Text.Parsec
import           Text.Parsec.String

import           System.FilePath.Posix ((</>))

import           CoreTypes
import           Parser
import           Parser.Gen
import           Parser.Internal
import           Parser.Types

import           Parser.TestUtils
import           TestUtils

spec :: Spec
spec = parallel $ do
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
            forAll (listOf generateSpace) (\ss -> f ss)
        it "fails for tabs" $ do
            forAll (listOf generateTab) (\ss -> f ss)
        it "fails for linespace" $ do
            forAll generateLineSpace (\ls -> f ls)

    describe "linespace" $ do
        let s = shouldSucceed whitespace
        it "succeeds for spaces" $ do
            forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do
            forAll (listOf generateTab) s
        it "succeeds for mixtures of spaces and tabs" $ do
            forAll generateLineSpace s

        let f = shouldSucceed whitespace
        it "fails for line ending characters" $ do
            forAll (listOf $ oneof [generateCarriageReturn, generateLineFeed]) f
        it "fails for any non-linespace, even if there's linespace in it" $  do
            forAll (listOf1 generateNormalCharacter) (\ls ->
             forAll generateLineSpace (\ls1 ->
              forAll generateLineSpace (\ls2 ->
               shouldFail linespace (ls1 ++ ls ++ ls2))))


    describe "whitespace" $ do
        let s = shouldSucceed whitespace
        it "succeeds for spaces" $ do
            forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do
            forAll (listOf generateTab) s
        it "succeeds carriage returns" $ do
            forAll (listOf generateCarriageReturn) s
        it "succeeds line feeds" $ do
            forAll (listOf generateLineFeed) s
        it "succeeds for mixtures of spaces, tabs, carriage returns and line feeds" $ do
            forAll generateWhiteSpace s

        it "fails for any non-whitespace, even if there's whitespace in it" $  do
            forAll (listOf1 generateNormalCharacter) (\ls ->
                        forAll generateWhiteSpace (\ws1 ->
                         forAll generateWhiteSpace (\ws2 ->
                            shouldFail whitespace (ws1 ++ ls ++ ws2))))


    describe "inLineSpace" $ do
        it "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            forAll generateLineSpace (\ws1 ->
              forAll generateLineSpace (\ws2 ->
                forAll (listOf1 generateNormalCharacter) (\ls ->
                    parseShouldSucceedAs (inLineSpace $ string ls) (ws1 ++ ls ++ ws2) ls)))

    describe "inWhiteSpace" $ do
        it "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            forAll generateWhiteSpace (\ws1 ->
              forAll generateWhiteSpace (\ws2 ->
                forAll (listOf1 generateNormalCharacter) (\ls ->
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
        it "succeeds for generated plain identifiers" $ do
            forAll generatePlainIdentifier $ \(e, a) ->
                parseShouldSucceedAs plainIdentifier e a

        let pi = shouldSucceed plainIdentifier
        it "succeeds for these examples" $ do
            pi "bash"
            pi "card"
            pi ".bashrc"
            pi "xmonad.hs"

        it "fails for generated quoted identifiers" $ do
            forAll generateQuotedIdentifier $ \(e, _) ->
                shouldFail plainIdentifier e

    describe "quotedIdentifier" $ do
        it "succeeds for generated plain identifiers surrounded in quotes" $ do
            forAll generatePlainIdentifier $ \(e, a) ->
                parseShouldSucceedAs quotedIdentifier ("\"" ++ e ++ "\"") a
        it "succeeds for generated quoted identifiers" $ do
            forAll generateQuotedIdentifier $ \(e, a) ->
                parseShouldSucceedAs quotedIdentifier e a

        let pi i = parseShouldSucceedAs quotedIdentifier ("\"" ++ i ++ "\"") i
        it "succeeds for these examples" $ do
            pi "bashrc"
            pi "with spaces"

        it "fails for generated plain identifiers" $ do
            forAll generatePlainIdentifier $ \(e, _) ->
                shouldFail quotedIdentifier e

        it "fails for generated identifiers with just one quote" $ do
            forAll generatePlainIdentifier $ \(e, _) ->
                shouldFail quotedIdentifier ("\"" ++ e) .&&. shouldFail quotedIdentifier (e ++ "\"")

    describe "identifier" $ do
        it "succeeds for generated identifiers" $ do
            forAll generateIdentifier $ \(e, a) ->
                parseShouldSucceedAs identifier e a

commentParserTests :: Spec
commentParserTests = do
    describe "eatComments" $ do
        it "should succeed unchanged on anything without comments" $ do
            property $ \s -> (not $ succeedsAnywhere comment s) ==> parseShouldSucceedAs eatComments s s

        let (-=>) e a = parseShouldSucceedAs eatComments e a
        it "successfully removes comments in these strings" $ do
            "abc#def\nghi"                          -=> "abcghi"
            "abc# This is a bigger comment \r\nghi" -=> "abcghi"
            "abc[[def]]ghi"                         -=> "abcghi"
            "abc[[ This is a bigger comment ]]ghi"  -=> "abcghi"
            "Heavy[[use]]of#comments\n."            -=> "Heavyof."

    describe "notComment" $ do
        it "should succeed for any string that doesn't contain comments" $ do
            property $ \s -> (not $ succeedsAnywhere comment s) ==> parseShouldSucceedAs notComment s s

    describe "lineComment" $ do
        it "succeeds for generated line comments" $ do
            forAll generateLineComment $ \(e, a) -> parseShouldSucceedAs lineComment e a
        it "succeeds for these test cases" $ do
            parseShouldSucceedAs lineComment "#a\n" "a"
            parseShouldSucceedAs lineComment "# This is a comment\n" " This is a comment"
            parseShouldSucceedAs lineComment "## This is a comment with two comment signs.\n" "# This is a comment with two comment signs."
            parseShouldSucceedAs lineComment "# with other eol\r\n" " with other eol"

    describe "blockComment" $ do
        it "succeeds for generated block comments" $ do
            forAll generateBlockComment $ \(e, a) -> parseShouldSucceedAs blockComment e a
        it "succeeds of these test cases" $ do
            parseShouldSucceedAs blockComment "[[a]]" "a"
            parseShouldSucceedAs blockComment "[[ This is a block comment. ]]" " This is a block comment. "
            parseShouldSucceedAs blockComment "[[ [This is a [block] comment containing brackets.] ]]" " [This is a [block] comment containing brackets.] "

    describe "comment" $ do
        it "succeeds for generated line comments" $ do
            forAll generateLineComment $ \(e, a) -> parseShouldSucceedAs comment e a

        it "succeeds for generated block comments" $ do
            forAll generateBlockComment $ \(e, a) -> parseShouldSucceedAs comment e a

pathParserTests :: Spec
pathParserTests = do
    describe "filepath" $ do
        it "succeeds for generated filepaths" $ do
            forAll generateFilePath $ \(e, a) -> parseShouldSucceedAs filepath e a
        it "succeeds for this quoted filepath with a space in it" $ do
            parseShouldSucceedAs filepath "\"/home/user/with spaces\"" "/home/user/with spaces"

        let s = shouldSucceed filepath
        it "succeeds for this file without an extension" $ do
            s "withoutExtension"
        it "succeeds for this simple file" $ do
            s "test.txt"
        it "succeeds for this simple file with a long extension" $ do
            s "file.somelongextension"
        it "succeeds for this absolute filepath" $ do
            s "/home/user/test.txt"
        it "succeeds for this absolute filepath with a long extension" $ do
            s "/home/user/file.somelongextension"
        it "succeeds for this absolute filepath with multiple extensions" $ do
            s "/home/user/test.multiple.extensions"
        it "succeeds for this relative filepath with a double dot" $ do
            s "/home/user/../user/test.txt"

    describe "directory" $ do
        it "succeeds for generated directories" $ do
            forAll generateDirectory $ \(e, a) -> parseShouldSucceedAs directory e a

        let s = shouldSucceed filepath
        it "succeeds for the home directory" $ do
            s "~"
        it "succeeds for this relative directory" $ do
            s "directory"
        it "succeeds for this absolute directory" $ do
            s "/home/user"
        it "succeeds for these directories in the home directory" $ do
            s "~/.vim"
            s "~/Dropbox"
            s "~/.xmonad"


declarationParserTests :: Spec
declarationParserTests = do
    describe "cardName" $ do
        it "succeeds on every card name that we generate" $ do
            forAll generateCardName $ \(a, e) -> parseShouldSucceedAs cardName a e

    describe "card" $ do
        let pc = parseShouldSucceedAs card
        it "succeeds on this card with an empty name correctly" $ do
            pc "card \"\" {}" $ Card "" (Block [])

        it "succeeds on this compressed empty cards" $ do
            forAll generateCardName $ \(a, e) ->
                parseShouldSucceedAs card ("card" ++ a ++ "{}") $ Card e (Block [])

        it "succeeds on empty cards with whitespace around the name" $ do
            forAll generateCardName $ \(a, e) ->
                forAll (twice generateWhiteSpace) $ \(ws1, ws2) ->
                    parseShouldSucceedAs card ("card" ++ ws1 ++ a ++ ws2 ++ "{}") $ Card e (Block [])

        it "succeeds on empty cards with whitespace between the brackets" $ do
            forAll generateCardName $ \(a, e) ->
                forAll generateWhiteSpace $ \ws ->
                    parseShouldSucceedAs card ("card" ++ a ++ "{" ++ ws ++ "}") $ Card e (Block [])

        it "fails on any card with an empty body" $ do
            forAll generateCardName $ \(a, _) ->
                forAll generateWhiteSpace $ \ws ->
                    shouldFail card ("card" ++ a ++ ws)

        it "succeeds on this complicated example" $ do
            parseShouldSucceedAs card ("card complicated {\n  alternatives $(HOST) shared\n  hello l-> goodbye\n into $(HOME)\n  outof depot\n  spark card othercard\n  kind link\n  {\n    one c-> more\n    source -> destination\n    file\n  }\n}")
                $ Card "complicated" $ Block
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
    describe "alternatives" $ do
        pend

    describe "deploymentKind" $ do
        let (-=>) = parseShouldSucceedAs deploymentKind
        it "succeeds for the link deployment kind" $ do
            "l->" -=> Just LinkDeployment
        it "succeeds for the copy deployment kind" $ do
            "c->" -=> Just CopyDeployment
        it "succeeds for the default deployment kind" $ do
            "->" -=> Nothing
        it "fails for anything else" $ do
            property $ \s -> (not $ any (== s) ["l->", "c->", "->"]) ==> shouldFail deploymentKind s


toplevelParserTests :: Spec
toplevelParserTests = do
    describe "sparkFile" $ do
        pend

    describe "resetPosition" $ do
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



