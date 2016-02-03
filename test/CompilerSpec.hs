module CompilerSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either           (isLeft, isRight)
import           Data.List             (intercalate, isPrefixOf)
import           System.FilePath.Posix ((</>))

import           CoreTypes
--import           Compiler.TestUtils
import           Compiler
import           Compiler.Internal
import           Compiler.TestUtils
import           Compiler.Types
import           Config
import           Parser.Types
import           TestUtils
import           Types

spec :: Spec
spec = parallel $ do
    singleCompileDec
    compilerBlackBoxTests

defaultCompilerState :: CompilerState
defaultCompilerState = CompilerState
    { state_deployment_kind_override = Nothing
    , state_into = ""
    , state_outof_prefix = []
    }

singleCompileDec :: Spec
singleCompileDec = describe "compileDec" $ do
    let s = defaultCompilerState
    let c = defaultConfig
    let sc = singleShouldCompileTo c s
    let sf = singleShouldFail c s

    let nonNull = arbitrary `suchThat` (not . null)
    let validFilePath = nonNull `suchThat` (not . containsNewlineCharacter)
    let easyFilePath = validFilePath `suchThat` (not . isPrefixOf ".")

    describe "Deploy" $ do
        it "uses the exact right text in source and destination when given valid filepaths without a leading dot" $ do
            forAll easyFilePath $ \from ->
                forAll easyFilePath $ \to ->
                    sc (Deploy from to Nothing) (Put [from] to LinkDeployment)

        it "handles filepaths with a leading dot correctly" $ do
            pending

        it "normalizes \'..\' in both source and destination" $ do
            pending

        it "fails on deployments with empty sources" $ do
            forAll nonNull $ \t ->
                sf (Deploy [] t Nothing)

        it "fails on deployments with empty destinations" $ do
            forAll nonNull $ \t ->
                sf (Deploy t [] Nothing)

        it "fails on deployments with empty source and destination" $ do
            sf (Deploy [] [] Nothing)

        it "fails on deployments with a newline character in the source" $ do
            forAll (nonNull `suchThat` containsNewlineCharacter) $ \from ->
                forAll validFilePath $ \to ->
                    sf (Deploy from to Nothing)

        it "fails on deployments with a newline character in the destination" $ do
            forAll validFilePath $ \from ->
                forAll (nonNull `suchThat` containsNewlineCharacter) $ \to ->
                    sf (Deploy from to Nothing)

        it "figures out the correct paths in these cases with default config and initial state" $ do
            let d = (Deploy "from" "to" $ Just LinkDeployment)
            sc d (Put ["from"] "to" LinkDeployment)


        it "uses the alternates correctly" $ do
            pending

        it "uses the into's correctly" $ do
            pending

        it "uses the outof's correctly" $ do
            pending


    describe "SparkOff" $ do
        it "adds a single card file reference to the list of cards to spark later" $ do
            forAll validFilePath $ \f ->
                let cr = CardFile $ CardFileReference f Nothing
                    d = SparkOff cr
                in compileSingleDec d s c `shouldBe` Right (s, ([], [cr]))

        it "adds any card reference to the list" $ do
            pending



compilerBlackBoxTests :: Spec
compilerBlackBoxTests = do

    let tr = "test_resources"
    describe "Correct succesful compile examples" $ do
        let dirs = map (tr </>) ["shouldCompile"]
        forFileInDirss dirs $ concerningContents $ \f contents -> do
            it f $ do
                r <- flip runReaderT defaultConfig $ runExceptT $ compileJob $ CardFileReference f Nothing
                r `shouldSatisfy` isRight

    describe "Correct unsuccesfull compile examples" $ do
        let dirs = map (tr </>) ["shouldNotParse", "shouldNotCompile"]
        forFileInDirss dirs $ concerningContents $ \f contents -> do
            it f $ do
                r <- flip runReaderT defaultConfig $ runExceptT $ compileJob $ CardFileReference f Nothing
                r `shouldSatisfy` isLeft

