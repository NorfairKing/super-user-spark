module CompilerSpec where

import           Test.Hspec
import           Test.QuickCheck

import           CoreTypes
--import           Compiler.TestUtils
import           Compiler
import           Compiler.Internal
import           Compiler.Types
import           Parser.Types
import           TestUtils
import           Types

spec :: Spec
spec = parallel $ do
    singleCompileDec



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
            let a = runIdentity $ runReaderT (runExceptT $ runWriterT $ execStateT (compileDec d) s) c
                    :: Either CompileError (CompilerState, ([Deployment], [CardReference]))
            a `shouldBe` Right (s, ([Put ["from"] "to" LinkDeployment], []))
    describe "SparkOff" $ do
        it "adds a single card reference to the list of cards to spark later" $ do
            pending
