module DeployerSpec where

import           Check.Internal
import           Check.Types
import           Config
import           Config.Types
import           Deployer.Internal
import           Monad
import           System.Directory
import           System.Posix.Files
import           Test.Hspec

spec :: Spec
spec = do
    cleanSpec

cleanSpec :: Spec
cleanSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing True sandbox
    let teardown = removeDirectoryRecursive sandbox

    let clean :: SparkConfig -> CleanupInstruction -> IO ()
        clean conf ci = runSparker conf (performClean ci) `shouldReturn` Right ()

    beforeAll_ setup $ afterAll_ teardown $ do
        describe "performClean" $ do

            it "doesn't remove this file if that's not in the config" $ do
                let c = defaultConfig { conf_deploy_replace_files = False }
                withCurrentDirectory sandbox $ do
                    let file = "test.txt"
                    writeFile file "This is a test"
                    diagnoseFp file `shouldReturn` IsFile
                    clean c $ CleanFile file
                    diagnoseFp file `shouldReturn` IsFile
                    removeFile file
                    diagnoseFp file `shouldReturn` Nonexistent

            it "removes this file if that's in the config" $ do
                let c = defaultConfig { conf_deploy_replace_files = True }
                withCurrentDirectory sandbox $ do
                    let file = "test.txt"
                    writeFile file "This is a test"
                    diagnoseFp file `shouldReturn` IsFile
                    clean c $ CleanFile file
                    diagnoseFp file `shouldReturn` Nonexistent

            it "doesn't remove this directory if that's not in the config" $ do
                let c = defaultConfig { conf_deploy_replace_directories = False }
                withCurrentDirectory sandbox $ do
                    let dir = "testdirectory"
                    createDirectoryIfMissing True dir
                    diagnoseFp dir `shouldReturn` IsDirectory
                    clean c $ CleanDirectory dir
                    diagnoseFp dir `shouldReturn` IsDirectory
                    removeDirectoryRecursive dir
                    diagnoseFp dir `shouldReturn` Nonexistent

            it "removes this directory if that's in the config" $ do
                let c = defaultConfig { conf_deploy_replace_directories = True }
                withCurrentDirectory sandbox $ do
                    let dir = "testdirectory"
                    createDirectoryIfMissing True dir
                    diagnoseFp dir `shouldReturn` IsDirectory
                    clean c $ CleanDirectory dir
                    diagnoseFp dir `shouldReturn` Nonexistent

            it "doesn't remove this link if that's not in the config" $ do
                let c = defaultConfig { conf_deploy_replace_links = False }
                withCurrentDirectory sandbox $ do
                    let link = "testlink"
                    let file = "testfile"
                    writeFile file "This is a test"
                    createSymbolicLink file link
                    diagnoseFp link `shouldReturn` IsLinkTo file
                    clean c $ CleanLink link
                    diagnoseFp link `shouldReturn` IsLinkTo file
                    removeLink link
                    diagnoseFp link `shouldReturn` Nonexistent
                    removeFile file
                    diagnoseFp file `shouldReturn` Nonexistent

            it "removes this link with an existent source if that's in the config" $ do
                let c = defaultConfig { conf_deploy_replace_links = True }
                withCurrentDirectory sandbox $ do
                    let link = "testlink"
                    let file = "testfile"
                    writeFile file "This is a test"
                    createSymbolicLink file link
                    diagnoseFp link `shouldReturn` IsLinkTo file
                    clean c $ CleanLink link
                    diagnoseFp link `shouldReturn` Nonexistent
                    removeFile file
                    diagnoseFp file `shouldReturn` Nonexistent

            it "removes this link with a nonexistent source if that's in the config" $ do
                let c = defaultConfig { conf_deploy_replace_links = True }
                withCurrentDirectory sandbox $ do
                    let link = "testlink"
                    let file = "testfile"
                    createSymbolicLink file link
                    diagnoseFp link `shouldReturn` IsLinkTo file
                    diagnoseFp file `shouldReturn` Nonexistent
                    clean c $ CleanLink link
                    diagnoseFp link `shouldReturn` Nonexistent
                    diagnoseFp file `shouldReturn` Nonexistent
