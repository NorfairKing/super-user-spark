module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad         (forM, forM_)
import           Data.Either           (isLeft, isRight)

import           Text.Parsec
import           Text.Parsec.String

import           System.Directory      (getDirectoryContents)
import           System.FilePath.Posix ((<.>), (</>))

import           Parser
import           Parser.Types

shouldSucceed :: (Show a, Eq a) => Parser a -> String -> IO ()
shouldSucceed parser input = input `shouldSatisfy` succeeds parser

shouldFail :: (Show a, Eq a) => Parser a -> String -> IO ()
shouldFail parser input = input `shouldNotSatisfy` succeeds parser

succeeds :: (Show a, Eq a) => Parser a -> String -> Bool
succeeds parser input = isRight $ parseWithoutSource (parser >> eof) input

fails :: (Show a, Eq a) => Parser a -> String -> Bool
fails parser input = not $ succeeds parser input

parseShouldSucceedAs :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
parseShouldSucceedAs parser input a = parseFromSource parser (show input) input `shouldBe` Right a

parseShouldBe :: (Show a, Eq a) => Parser a -> String -> Either ParseError a -> IO ()
parseShouldBe parser input result = parseFromSource parser (show input) input `shouldBe` result

parseWithoutSource :: Parser a -> String -> Either ParseError a
parseWithoutSource parser input = parseFromSource parser "Test input" input

generateLetter :: Gen Char
generateLetter = elements $ ['a'..'z'] ++ ['A'..'Z']

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

spec :: Spec
spec = do
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

        let mf c = property $ (\i -> f $ replicate i c)
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

    describe "inBraces" $ do
        it "succeeds for cases where we enclose braces around a string without braces" $ do
            forAll (listOf1 arbitrary `suchThat` (\c -> c /= "{" && c /= "}")) (\word ->
                parseShouldSucceedAs (inBraces $ string word) ("{" ++ word ++ "}") word)

    describe "inQuotes" $ do
        it "succeeds for cases where we enclose quotes around a string without quotes" $ do
            forAll (listOf1 arbitrary `suchThat` (/= "\"")) (\word ->
                parseShouldSucceedAs (inQuotes $ string word) ("\"" ++ word ++ "\"") word)

    describe "delim" $ do
        it "succeeds on a semicolon" $ do
            shouldSucceed delim ";"
        it "succeeds on an eol" $ do
            once $ forAll (arbitrary `suchThat` succeeds eol) (shouldSucceed delim)


    let tr = "test_resources"
    describe "positive black box parse tests" $ do
        let dirs = map (tr </>) ["shouldParse", "shouldCompile", "shouldNotCompile"]
        forFiles dirs $ \f -> do
            it (f ++ " correctly gets parsed succesfully") $ do
                contents <- readFile $ f
                parseFromSource sparkFile f contents `shouldSatisfy` isRight

    describe "negative black box parse tests" $ do
        let dir = tr </> "shouldNotParse"
        forFiles [dir] $ \f -> do
            it (f ++ " correctly gets parsed unsuccesfully") $ do
                contents <- readFile f
                parseFromSource sparkFile f contents `shouldSatisfy` isLeft

forFiles :: [FilePath] -> (FilePath -> SpecWith a) -> SpecWith a
forFiles dirs func = do
    allFiles <- fmap concat $ forM dirs $ \dir -> do
        files <- runIO (getDirectoryContents dir)
        let ffiles = filter (`notElem` [".", ".."]) files
        return $ map (dir </>) ffiles
    forM_ allFiles func
