{-# LANGUAGE TypeApplications #-}

module SuperUserSpark.ParserSpec where

import TestImport hiding (succeeds)

import Data.Either (isLeft, isRight)
import Data.List (intercalate, isInfixOf)
import Text.Parsec

import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Gen ()
import SuperUserSpark.Language.Types
import SuperUserSpark.Parser.Gen
import SuperUserSpark.Parser.Internal
import SuperUserSpark.Parser.TestUtils
import TestUtils

spec :: Spec
spec =
    parallel $ do
        instanceSpec
        blankspaceParserTests
        enclosingCharacterTests
        delimiterTests
        identifierParserTests
        commentParserTests
        pathParserTests
        declarationParserTests
        parserBlackBoxTests
        toplevelParserTests

enclosingCharacterTests :: Spec
enclosingCharacterTests = do
    describe "inBraces" $ do
        it
            "succeeds for cases where we enclose braces around a string without braces" $ do
            forAll
                (listOf1 arbitrary `suchThat` (\c -> c /= "{" && c /= "}"))
                (\word ->
                     parseShouldSucceedAs
                         (inBraces $ string word)
                         ("{" ++ word ++ "}")
                         word)
    describe "inQuotes" $ do
        it
            "succeeds for cases where we enclose quotes around a string without quotes" $ do
            forAll (listOf1 arbitrary `suchThat` (/= "\"")) $ \word ->
                parseShouldSucceedAs
                    (inQuotes $ string word)
                    ("\"" ++ word ++ "\"")
                    word

instanceSpec :: Spec
instanceSpec = do
    eqSpec @Card
    genValidSpec @Card
    eqSpec @Declaration
    genValidSpec @Declaration
    eqSpec @CardNameReference
    genValidSpec @CardNameReference
    eqSpec @CardFileReference
    genValidSpec @CardFileReference
    eqSpec @CardReference
    genValidSpec @CardReference
    eqSpec @SparkFile
    genValidSpec @SparkFile

blankspaceParserTests :: Spec
blankspaceParserTests = do
    describe "eol" $ do
        let s = shouldSucceed eol
        it "succeeds for Linux line endings" $ do s "\n"
        it "succeeds for Windows line endings" $ do s "\r\n"
        it "succeeds for Mac line endings" $ do s "\r"
        let f = shouldFail eol
        it "fails for the empty string" $ do f ""
        it "fails for spaces" $ do forAll (listOf generateSpace) (\ss -> f ss)
        it "fails for tabs" $ do forAll (listOf generateTab) (\ss -> f ss)
        it "fails for linespace" $ do forAll generateLineSpace (\ls -> f ls)
    describe "linespace" $ do
        let s = shouldSucceed linespace
        it "succeeds for spaces" $ do forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do forAll (listOf generateTab) s
        it "succeeds for mixtures of spaces and tabs" $ do
            forAll generateLineSpace s
        let f = shouldSucceed whitespace
        it "fails for line ending characters" $ do
            forAll (listOf $ oneof [generateCarriageReturn, generateLineFeed]) f
        it "fails for any non-linespace, even if there's linespace in it" $
            forAll (listOf1 generateNormalCharacter) $ \ls ->
                forAll generateLineSpace $ \ls1 ->
                    forAll generateLineSpace $ \ls2 ->
                        shouldFail linespace (ls1 ++ ls ++ ls2)
    describe "whitespace" $ do
        let s = shouldSucceed whitespace
        it "succeeds for spaces" $ do forAll (listOf generateSpace) s
        it "succeeds for tabs" $ do forAll (listOf generateTab) s
        it "succeeds carriage returns" $ do
            forAll (listOf generateCarriageReturn) s
        it "succeeds line feeds" $ do forAll (listOf generateLineFeed) s
        it
            "succeeds for mixtures of spaces, tabs, carriage returns and line feeds" $ do
            forAll generateWhiteSpace s
        it "fails for any non-whitespace, even if there's whitespace in it" $ do
            forAll (listOf1 generateNormalCharacter) $ \ls ->
                forAll generateWhiteSpace $ \ws1 ->
                    forAll generateWhiteSpace $ \ws2 ->
                        shouldFail whitespace (ws1 ++ ls ++ ws2)
    describe "inLineSpace" $ do
        it
            "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            forAll generateLineSpace $ \ws1 ->
                forAll generateLineSpace $ \ws2 ->
                    forAll (listOf1 generateNormalCharacter) $ \ls ->
                        parseShouldSucceedAs
                            (inLineSpace $ string ls)
                            (ws1 ++ ls ++ ws2)
                            ls
    describe "inWhiteSpace" $ do
        it
            "succeeds for cases where we append whitespace to the front and back of non-whitespace" $ do
            forAll generateWhiteSpace $ \ws1 ->
                forAll generateWhiteSpace $ \ws2 ->
                    forAll (listOf1 generateNormalCharacter) $ \ls ->
                        parseShouldSucceedAs
                            (inWhiteSpace $ string ls)
                            (ws1 ++ ls ++ ws2)
                            ls

delimiterTests :: Spec
delimiterTests = do
    describe "delim" $ do
        it "succeeds on a semicolon" $ do shouldSucceed delim ";"
        it "succeeds on an eol" $ do
            once $
                forAll (arbitrary `suchThat` succeeds eol) (shouldSucceed delim)

identifierParserTests :: Spec
identifierParserTests = do
    describe "plainIdentifier" $ do
        it "succeeds for generated plain identifiers" $ do
            forAll generatePlainIdentifier $ \(e, a) ->
                parseShouldSucceedAs plainIdentifier e a
        let pti = shouldSucceed plainIdentifier
        it "succeeds for these examples" $ do
            pti "bash"
            pti "card"
            pti ".bashrc"
            pti "xmonad.hs"
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
        let pti i = parseShouldSucceedAs quotedIdentifier ("\"" ++ i ++ "\"") i
        it "succeeds for these examples" $ do
            pti "bashrc"
            pti "with spaces"
        it "fails for generated plain identifiers" $ do
            forAll generatePlainIdentifier $ \(e, _) ->
                shouldFail quotedIdentifier e
        it "fails for generated identifiers with just one quote" $ do
            forAll generatePlainIdentifier $ \(e, _) ->
                shouldFail quotedIdentifier ("\"" ++ e) .&&.
                shouldFail quotedIdentifier (e ++ "\"")
    describe "identifier" $ do
        it "succeeds for generated identifiers" $ do
            forAll generateIdentifier $ \(e, a) ->
                parseShouldSucceedAs identifier e a

commentParserTests :: Spec
commentParserTests = do
    describe "eatComments" $ do
        it "should succeed unchanged on anything without comments" $ do
            property $ \s ->
                (not $ succeedsAnywhere comment s) ==>
                parseShouldSucceedAs eatComments s s
        let (-=>) e a = parseShouldSucceedAs eatComments e a
        it "successfully removes comments in these strings" $ do
            "abc#def\nghi" -=> "abcghi"
            "abc# This is a bigger comment \r\nghi" -=> "abcghi"
            "abc[[def]]ghi" -=> "abcghi"
            "abc[[ This is a bigger comment ]]ghi" -=> "abcghi"
            "Heavy[[use]]of#comments\n." -=> "Heavyof."
    describe "notComment" $ do
        it "should succeed for any string that doesn't contain comments" $ do
            property $ \s ->
                (not $ succeedsAnywhere comment s) ==>
                parseShouldSucceedAs notComment s s
    describe "lineComment" $ do
        it "succeeds for generated line comments" $ do
            forAll generateLineComment $ \(e, a) ->
                parseShouldSucceedAs lineComment e a
        it "succeeds for these test cases" $ do
            parseShouldSucceedAs lineComment "#a\n" "a"
            parseShouldSucceedAs
                lineComment
                "# This is a comment\n"
                " This is a comment"
            parseShouldSucceedAs
                lineComment
                "## This is a comment with two comment signs.\n"
                "# This is a comment with two comment signs."
            parseShouldSucceedAs
                lineComment
                "# with other eol\r\n"
                " with other eol"
    describe "blockComment" $ do
        it "succeeds for generated block comments" $ do
            forAll generateBlockComment $ \(e, a) ->
                parseShouldSucceedAs blockComment e a
        it "succeeds of these test cases" $ do
            parseShouldSucceedAs blockComment "[[a]]" "a"
            parseShouldSucceedAs
                blockComment
                "[[ This is a block comment. ]]"
                " This is a block comment. "
            parseShouldSucceedAs
                blockComment
                "[[ [This is a [block] comment containing brackets.] ]]"
                " [This is a [block] comment containing brackets.] "
    describe "comment" $ do
        it "succeeds for generated line comments" $ do
            forAll generateLineComment $ \(e, a) ->
                parseShouldSucceedAs comment e a
        it "succeeds for generated block comments" $ do
            forAll generateBlockComment $ \(e, a) ->
                parseShouldSucceedAs comment e a

pathParserTests :: Spec
pathParserTests = do
    describe "filepath" $ do
        it "succeeds for generated filepaths" $ do
            forAll generateFilePath $ \(e, a) ->
                parseShouldSucceedAs filepath e a
        it "succeeds for this quoted filepath with a space in it" $ do
            parseShouldSucceedAs
                filepath
                "\"/home/user/with spaces\""
                "/home/user/with spaces"
        let s = shouldSucceed filepath
        it "succeeds for this file without an extension" $ do
            s "withoutExtension"
        it "succeeds for this simple file" $ do s "test.txt"
        it "succeeds for this simple file with a long extension" $ do
            s "file.somelongextension"
        it "succeeds for this absolute filepath" $ do s "/home/user/test.txt"
        it "succeeds for this absolute filepath with a long extension" $ do
            s "/home/user/file.somelongextension"
        it "succeeds for this absolute filepath with multiple extensions" $ do
            s "/home/user/test.multiple.extensions"
        it "succeeds for this relative filepath with a double dot" $ do
            s "/home/user/../user/test.txt"
        let f = shouldFail filepath
        it "fails for just a slash" $ do f "/"
        it "fails for strings ending in a slash" $ do
            property $ \s_ -> f (s_ ++ "/")
    describe "directory" $ do
        it "succeeds for generated directories" $ do
            forAll generateDirectory $ \(e, a) ->
                parseShouldSucceedAs directory e a
        let s = shouldSucceed directory
        it "succeeds for the home directory" $ do s "~"
        it "succeeds for this relative directory" $ do s "directory"
        it "succeeds for this absolute directory" $ do s "/home/user"
        it "succeeds for these directories in the home directory" $ do
            s "~/.vim"
            s "~/Dropbox"
            s "~/.xmonad"
        let f = shouldFail directory
        it "fails for just a slash" $ do f "/"
        it "fails for strings ending in a slash" $ do
            property $ \s_ -> f (s_ ++ "/")

declarationParserTests :: Spec
declarationParserTests = do
    describe "cardName" $ do
        it "succeeds on every card name that we generate" $ do
            forAll generateCardName $ \(a, e) ->
                parseShouldSucceedAs cardNameP a e
    describe "card" $ do
        let pc = parseShouldSucceedAs card
        it "succeeds on this card with an empty name correctly" $ do
            pc "card \"\" {}" $ Card "" (Block [])
        it "succeeds on this compressed empty cards" $ do
            forAll generateCardName $ \(a, e) ->
                parseShouldSucceedAs card ("card" ++ a ++ "{}") $
                Card e (Block [])
        it "succeeds on empty cards with whitespace around the name" $ do
            forAll generateCardName $ \(a, e) ->
                forAll (twice generateWhiteSpace) $ \(ws1, ws2) ->
                    parseShouldSucceedAs
                        card
                        ("card" ++ ws1 ++ a ++ ws2 ++ "{}") $
                    Card e (Block [])
        it "succeeds on empty cards with whitespace between the brackets" $ do
            forAll generateCardName $ \(a, e) ->
                forAll generateWhiteSpace $ \ws ->
                    parseShouldSucceedAs card ("card" ++ a ++ "{" ++ ws ++ "}") $
                    Card e (Block [])
        it "fails on any card with an empty body" $ do
            forAll generateCardName $ \(a, _) ->
                forAll generateWhiteSpace $ \ws ->
                    shouldFail card ("card" ++ a ++ ws)
        it "succeeds on this complicated example" $ do
            parseShouldSucceedAs
                card
                ("card complicated {\n  alternatives $(HOST) shared\n  hello l-> goodbye\n into $(HOME)\n  outof depot\n  spark card othercard\n  kind link\n  {\n    one c-> more\n    source -> destination\n    file\n  }\n}") $
                Card "complicated" $
                Block
                    [ Alternatives ["$(HOST)", "shared"]
                    , Deploy "hello" "goodbye" (Just LinkDeployment)
                    , IntoDir "$(HOME)"
                    , OutofDir "depot"
                    , SparkOff (CardName (CardNameReference "othercard"))
                    , DeployKindOverride LinkDeployment
                    , Block
                          [ Deploy "one" "more" (Just CopyDeployment)
                          , Deploy "source" "destination" Nothing
                          , Deploy "file" "file" Nothing
                          ]
                    ]
    describe "declarations" $ do
        it "succeeds for generated declarations'" $ do pending
        let s = parseShouldSucceedAs declarations
        it "succeeds for these cases" $ do
            s "into dir;outof dir" [IntoDir "dir", OutofDir "dir"]
    describe "declaration" $ do
        it "succeeds for generated declarations" $ do pending
        let s = parseShouldSucceedAs declaration
        it "succeeds for these cases" $ do
            s "into directory" (IntoDir "directory")
            s "outof \"other directory\"" (OutofDir "other directory")
            s "{}" (Block [])
            s "{{{};{};{}}}" (Block [Block [Block [], Block [], Block []]])
            s
                "\"hi i'm a file\"c->iamthedestination"
                (Deploy
                     "hi i'm a file"
                     "iamthedestination"
                     (Just CopyDeployment))
    describe "block" $ do
        it "succeeds for empty blocks" $ do
            parseShouldSucceedAs block "{}" (Block [])
        it "succeeds for a doubly nested empty block" $ do
            parseShouldSucceedAs block "{{}}" (Block [Block []])
        it "succeeds for a triply nested empty block" $ do
            parseShouldSucceedAs block "{{{}}}" (Block [Block [Block []]])
        let s = parseShouldSucceedAs block
        it "succeeds for these cases" $ do
            s
                "{into ~;bashrc -> .bashrc}"
                (Block [IntoDir "~", Deploy "bashrc" ".bashrc" Nothing])
            s
                "{\n    into \"~\"\n    \"xmonad\" -> \".xmonad\"\n}"
                (Block [IntoDir "~", Deploy "xmonad" ".xmonad" Nothing])
    describe "sparkOff" $ do
        it "succeeds for generated sparkOff declarations" $ do pending
        let s f g =
                parseShouldSucceedAs
                    sparkOff
                    f
                    (SparkOff $ CardName $ CardNameReference g)
        it "succeeds for these cases" $ do
            s "spark card name" "name"
            s "sparkcardname" "name"
            s "spark card \"name with spaces\"" "name with spaces"
    describe "intoDir" $ do
        it "succeeds for generated into declarations" $ do
            forAll generateLineSpace $ \ls ->
                forAll generateDirectory $ \(d, da) ->
                    parseShouldSucceedAs
                        intoDir
                        ("into" ++ ls ++ d)
                        (IntoDir da)
        let s f g = parseShouldSucceedAs intoDir f (IntoDir g)
        it "succeeds for these cases" $ do
            s "into \"bash\"" "bash"
            s "into\t.xmonad" ".xmonad"
            s "into ~" "~"
    describe "outOfDir" $ do
        it "succeeds for generated outof declarations" $ do
            forAll generateLineSpace $ \ls ->
                forAll generateDirectory $ \(d, da) ->
                    parseShouldSucceedAs
                        outOfDir
                        ("outof" ++ ls ++ d)
                        (OutofDir da)
        let s f g = parseShouldSucceedAs outOfDir f (OutofDir g)
        it "succeeds for these cases" $ do
            s "outof bash" "bash"
            s "outof\t.xmonad" ".xmonad"
    describe "alternatives" $ do
        it "succeeds for generated alternatives declarations with single spaces" $ do
            forAll (listOf1 generateDirectory) $ \ds ->
                let (des, das) = unzip ds
                in parseShouldSucceedAs
                       alternatives
                       ("alternatives" ++ " " ++ intercalate " " des)
                       (Alternatives das)
    describe "deployment" $ do
        it "succeeds for short deployments" pending
        it "succeeds for long deployments" pending
    describe "shortDeployment" $ do
        it "succeeds for any filepath with an identity deployment" $ do
            property $ \f ->
                succeeds filepath f ==>
                parseShouldSucceedAs shortDeployment f (Deploy f f Nothing)
        it "succeeds for generated filepaths with an identity deployment" $ do
            forAll generateFilePath $ \(f, g) ->
                parseShouldSucceedAs shortDeployment f (Deploy g g Nothing)
        it "succeeds for any directory with an identity deployment" $ do
            property $ \f ->
                succeeds directory f ==>
                parseShouldSucceedAs shortDeployment f (Deploy f f Nothing)
        it "succeeds for generated directories with an identity deployment" $ do
            forAll generateDirectory $ \(f, g) ->
                parseShouldSucceedAs shortDeployment f (Deploy g g Nothing)
        let s f = parseShouldSucceedAs shortDeployment f (Deploy f f Nothing)
        it "succeeds as-is for these cases" $ do
            s "file.txt"
            s "xmonad.hs"
            s "/home/user/.bashrc"
    describe "longDeployment" $ do
        it "succeeds for generated long deployments with quoted identifiers" $ do
            forAll generateDeploymentKindSymbol $ \dks ->
                forAll generateLineSpace $ \ls1 ->
                    forAll generateLineSpace $ \ls2 ->
                        forAll generateQuotedIdentifier $ \(fp1, fp1a) ->
                            forAll generateQuotedIdentifier $ \(fp2, fp2a) ->
                                case parseWithoutSource deploymentKind dks of
                                    Left _ ->
                                        fail
                                            "There was a problem with parsing the deployment kind"
                                    Right dk ->
                                        parseShouldSucceedAs
                                            longDeployment
                                            (fp1 ++ ls1 ++ dks ++ ls2 ++ fp2)
                                            (Deploy fp1a fp2a dk)
        it
            "succeeds for single-space-separated long deployments with gerenated plain identifiers" $ do
            pendingWith
                "This would go wrong with plain identifiers they can end with \'l\' or \'c\'. Make sure to document this behaviour and write another test with plain identifiers."
        let s f g h i = parseShouldSucceedAs longDeployment f (Deploy g h i)
        it "succeeds for these cases" $ do
            s
                "\"something with spaces\"c->/home/user/test.txt"
                "something with spaces"
                "/home/user/test.txt"
                (Just CopyDeployment)
            s
                "\"xmonad.hs\"l-> /home/user/.xmonad/xmonad.hs"
                "xmonad.hs"
                "/home/user/.xmonad/xmonad.hs"
                (Just LinkDeployment)
            s
                "bashrc\t->\t/home/user/.bashrc"
                "bashrc"
                "/home/user/.bashrc"
                Nothing
    describe "deploymentKind" $ do
        let (-=>) = parseShouldSucceedAs deploymentKind
        it "succeeds for the link deployment kind" $ do
            "l->" -=> Just LinkDeployment
        it "succeeds for the copy deployment kind" $ do
            "c->" -=> Just CopyDeployment
        it "succeeds for the default deployment kind" $ do "->" -=> Nothing
        it
            "succeeds pipeDeploymentKind if the result is a pipe deployment but not otherwise" $ do
            forAll genValid $ \s ->
                case parseWithoutSource deploymentKind s of
                    Left err -> shouldFail pipeDeploymentKind s
                    Right res ->
                        case res of
                            Just (PipeDeployment command) ->
                                parseShouldSucceedAs pipeDeploymentKind s $
                                PipeDeployment command
                            _ -> shouldFail pipeDeploymentKind s
    describe "pipeDeploymentKind" $ do
        let (-=>) = parseShouldSucceedAs pipeDeploymentKind
        it
            "succeeds for a pipe deployment with arbitrary command that does not contain ']>'" $
            forAll (genValid `suchThat` (not . ("]>" `isInfixOf`))) $ \command ->
                ("-[" ++ command ++ "]>") -=> PipeDeployment command

cardReferenceParserTests :: Spec
cardReferenceParserTests = do
    describe "compilerCardReference" $ do pend
    describe "deployerCardReference" $ do pend
    describe "compiledCardReference" $ do pend
    describe "cardReference" $ do pend
    describe "cardNameReference" $ do
        pend
        let s f g =
                parseShouldSucceedAs cardNameReference f (CardNameReference g)
        it "succeeds for these cases" $ do
            s "card name" "name"
            s "cardname" "name"
            s "card \"name with spaces\"" "name with spaces"
    describe "cardFileReference" $ do
        pend
        let s = parseShouldSucceedAs cardFileReference
        it "succeeds for these cases" $ do
            s "file card.sus" (CardFileReference "card.sus" Nothing)
            s
                "file card.sus name"
                (CardFileReference "card.sus" $ Just $ CardNameReference "name")
    describe "unprefixedCardFileReference" $ do pend

parserBlackBoxTests :: Spec
parserBlackBoxTests = do
    testRecoursesDir <- runIO $ resolveDir' "test_resources"
    describe "Correct succesful parse examples" $ do
        let dirs =
                map
                    (testRecoursesDir </>)
                    [shouldParseDir, shouldCompileDir, shouldNotCompileDir]
        forFileInDirss dirs $
            concerningContents $ \f contents -> do
                it (toFilePath f) $
                    parseCardFile f contents `shouldSatisfy` isRight
    describe "Correct unsuccesfull parse examples" $ do
        let dirs = map (testRecoursesDir </>) [shouldNotParseDir]
        forFileInDirss dirs $
            concerningContents $ \f contents -> do
                it (toFilePath f) $
                    parseCardFile f contents `shouldSatisfy` isLeft

toplevelParserTests :: Spec
toplevelParserTests = do
    describe "sparkFile" $ do
        it "Only ever produces valid SparkFile's" $
            validIfSucceeds2 parseCardFile
        pend
    describe "resetPosition" $ do pend
