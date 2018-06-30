module SuperUserSpark.EndToEndSpec
    ( spec
    ) where

import TestImport hiding ((</>), removeFile, copyFile)

import qualified Prelude as P (writeFile, readFile)

import SuperUserSpark
import SuperUserSpark.Utils
import System.Directory
import System.Environment (withArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix ((</>))
import System.Posix.Files

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
    here <- runIO getCurrentDirectory
    let sandbox = here </> "test_sandbox"
    let setup = createDirectoryIfMissing True sandbox
    let teardown = removeDirectoryRecursive sandbox
    let rsc = here </> "test_resources" </> "end-to-end"
    beforeAll_ setup $
        afterAll_ teardown $
            describe "standard bash card test" $ do
                let bashrsc = rsc </> "bash.sus"
                let bashrscres = rsc </> "bash.sus.res"
                let cardfile = sandbox </> "bash.sus"
                let up = do
                        copyFile bashrsc cardfile
                        withCurrentDirectory sandbox $ do
                            createDirectoryIfMissing True "bash"
                            withCurrentDirectory "bash" $ do
                                P.writeFile "bash_aliases" "bash_aliases"
                                P.writeFile "bashrc" "bashrc"
                                P.writeFile "bash_profile" "bash_profile"
                let down = do
                        removeFile cardfile
                        withCurrentDirectory sandbox $
                            removeDirectoryRecursive "bash"
                beforeAll_ up $
                    afterAll_ down $ do
                        it "parses correcty" $
                            withCurrentDirectory sandbox $
                                withArgs ["parse", cardfile] spark `shouldReturn`
                                ()
                        it "compiles correctly" $ do
                            let outfile = sandbox </> "bash.sus.res"
                            withCurrentDirectory sandbox $
                                withArgs
                                    ["compile", cardfile, "--output", outfile]
                                    spark `shouldReturn`
                                ()
                            actual <- P.readFile outfile
                            expected <- P.readFile bashrscres
                            unless (actual == expected) $ expectationFailure $ unlines ["Expected and actual differ:", expected, actual]
                        it "checks without exceptions" $
                            withCurrentDirectory sandbox $
                                withArgs ["check", cardfile] spark `shouldReturn`
                                ()
                        it "deploys correctly" $
                            withCurrentDirectory sandbox $ do
                                withArgs ["deploy", cardfile] spark `shouldReturn`
                                    ()
                                let f1 = "subdir" </> ".bashrc"
                                    f2 = "subdir" </> ".bash_aliases"
                                    f3 = "subdir" </> ".bash_profile"
                                P.readFile f1 `shouldReturn` "bashrc"
                                P.readFile f2 `shouldReturn` "bash_aliases"
                                P.readFile f3 `shouldReturn` "bash_profile"
                                removeLink f1
                                removeLink f2
                                removeLink f3
