module EndToEndSpec
    ( spec
    ) where

import Spark
import System.Directory hiding (createDirectoryIfMissing)
import System.Environment (withArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix ((</>))
import System.Posix.Files
import Test.Hspec
import Utils

spec :: Spec
spec = do
    helpTextSpec
    regularWorkflowSpec

helpTextSpec :: Spec
helpTextSpec =
    describe "help text test" $ do
        it "shows the help text without crashing" $ do
            withArgs ["--help"] spark `shouldThrow` (== ExitSuccess)

regularWorkflowSpec :: Spec
regularWorkflowSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing sandbox
    let teardown = removeDirectoryRecursive sandbox
    let rsc = "test_resources" </> "end-to-end"
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "standard bash card test" $ do
                let bashrsc = rsc </> "bash.sus"
                let bashrscres = rsc </> "bash.sus.res"
                let cardfile = sandbox </> "bash.sus"
                let up = do
                        copyFile bashrsc cardfile
                        withCurrentDirectory sandbox $ do
                            createDirectoryIfMissing "bash"
                            withCurrentDirectory "bash" $ do
                                writeFile "bash_aliases" "bash_aliases"
                                writeFile "bashrc" "bashrc"
                                writeFile "bash_profile" "bash_profile"
                let down = do
                        removeFile cardfile
                        withCurrentDirectory sandbox $ do
                            removeDirectoryRecursive "bash"
                beforeAll_ up $
                    afterAll_ down $ do
                        it "parses correcty" $ do
                            withArgs ["parse", cardfile] spark `shouldReturn` ()
                        it "compiles correctly" $ do
                            let outfile = sandbox </> "bash.sus.res"
                            withArgs
                                ["compile", cardfile, "--output", outfile]
                                spark `shouldReturn`
                                ()
                            actual <- readFile outfile
                            expected <- readFile bashrscres
                            actual `shouldBe` expected
                        it "checks without exceptions" $ do
                            withArgs ["check", cardfile] spark `shouldReturn` ()
                        it "deploys correctly" $ do
                            withArgs ["deploy", cardfile] spark `shouldReturn`
                                ()
                            let f1 = "subdir" </> ".bashrc"
                                f2 = "subdir" </> ".bash_aliases"
                                f3 = "subdir" </> ".bash_profile"
                            readFile f1 `shouldReturn` "bashrc"
                            readFile f2 `shouldReturn` "bash_aliases"
                            readFile f3 `shouldReturn` "bash_profile"
                            removeLink f1
                            removeLink f2
                            removeLink f3
