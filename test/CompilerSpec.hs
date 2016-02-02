module CompilerSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either           (isLeft, isRight)
import           Data.List             (intercalate)
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


singleCompileDec :: Spec
singleCompileDec = describe "compileDec" $ do
    describe "Deploy" $ do
        it "figures out the correct paths in this case" $ do
            let d = (Deploy "from" "to" $ Just LinkDeployment)
            let s = CompilerState {
                  state_deployment_kind_override = Nothing
                , state_into = ""
                , state_outof_prefix = []
                } :: CompilerState
            let c = Config {
                  conf_compile_override = Nothing
                }
            let a = compileSingleDec d s c :: Either CompileError (CompilerState, ([Deployment], [CardReference]))
            a `shouldBe` Right (s, ([Put ["from"] "to" LinkDeployment], []))
    describe "SparkOff" $ do
        it "adds a single card reference to the list of cards to spark later" $ do
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

