module DeployerSpec where

import           Check.Internal
import           Check.Types
import           Config
import           Config.Types
import           Data.Either        (isLeft)
import           Data.Maybe         (isNothing)
import           Deployer.Internal
import           Deployer.Types
import           Monad
import           System.Directory   hiding (createDirectoryIfMissing)
import           System.Posix.Files
import           Test.Hspec
import           Test.QuickCheck
import           Utils

spec :: Spec
spec = do
    cleanSpec
    deploymentSpec
    completionSpec

cleanSpec :: Spec
cleanSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing sandbox
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
                    createDirectoryIfMissing dir
                    diagnoseFp dir `shouldReturn` IsDirectory
                    clean c $ CleanDirectory dir
                    diagnoseFp dir `shouldReturn` IsDirectory
                    removeDirectoryRecursive dir
                    diagnoseFp dir `shouldReturn` Nonexistent

            it "removes this directory if that's in the config" $ do
                let c = defaultConfig { conf_deploy_replace_directories = True }
                withCurrentDirectory sandbox $ do
                    let dir = "testdirectory"
                    createDirectoryIfMissing dir
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

deploymentSpec :: Spec
deploymentSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing sandbox
    let teardown = removeDirectoryRecursive sandbox

    beforeAll_ setup $ afterAll_ teardown $ do
        describe "copy" $ do
            it "succcesfully copies this file" $ do
                withCurrentDirectory sandbox $ do
                    let src = "testfile"
                    let dst = "testcopy"
                    writeFile src "This is a file."

                    diagnoseFp src `shouldReturn` IsFile
                    diagnoseFp dst `shouldReturn` Nonexistent

                    -- Under test
                    copy src dst

                    diagnoseFp src `shouldReturn` IsFile
                    diagnoseFp dst `shouldReturn` IsFile

                    dsrc <- diagnose src
                    ddst <- diagnose dst
                    diagnosedHashDigest ddst `shouldBe` diagnosedHashDigest dsrc

                    removeFile src
                    removeFile dst

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent

            it "succcesfully copies this directory" $ do
                withCurrentDirectory sandbox $ do
                    let src = "testdir"
                    let dst = "testcopy"
                    createDirectoryIfMissing src

                    diagnoseFp src `shouldReturn` IsDirectory
                    diagnoseFp dst `shouldReturn` Nonexistent

                    -- Under test
                    copy src dst

                    diagnoseFp src `shouldReturn` IsDirectory
                    diagnoseFp dst `shouldReturn` IsDirectory

                    dsrc <- diagnose src
                    ddst <- diagnose dst
                    diagnosedHashDigest ddst `shouldBe` diagnosedHashDigest dsrc

                    removeDirectoryRecursive src
                    removeDirectoryRecursive dst

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent

        describe "link" $ do
            it "successfully links this file" $ do
                withCurrentDirectory sandbox $ do
                    let src = "testfile"
                    let dst = "testlink"

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent

                    writeFile src "This is a test."

                    diagnoseFp src `shouldReturn` IsFile
                    diagnoseFp dst `shouldReturn` Nonexistent

                    -- Under test
                    link src dst

                    diagnoseFp src `shouldReturn` IsFile
                    diagnoseFp dst `shouldReturn` IsLinkTo src

                    removeFile src
                    removeLink dst

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent

            it "successfully links this directory" $ do
                withCurrentDirectory sandbox $ do
                    let src = "testdir"
                    let dst = "testlink"

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent

                    createDirectoryIfMissing src

                    diagnoseFp src `shouldReturn` IsDirectory
                    diagnoseFp dst `shouldReturn` Nonexistent

                    -- Under test
                    link src dst

                    diagnoseFp src `shouldReturn` IsDirectory
                    diagnoseFp dst `shouldReturn` IsLinkTo src

                    removeDirectoryRecursive src
                    removeLink dst

                    diagnoseFp src `shouldReturn` Nonexistent
                    diagnoseFp dst `shouldReturn` Nonexistent


completionSpec :: Spec
completionSpec = do
    describe "parseId" $ do
        it "Works for these cases" $ do
            parseId "" `shouldBe` []
            parseId "file" `shouldBe` [Plain "file"]
            parseId "something$(with)variable" `shouldBe` [Plain "something", Var "with", Plain "variable"]
            parseId "$(one)$(two)$(three)" `shouldBe` [Var "one", Var "two", Var "three"]

    describe "replaceId" $ do
        it "leaves plain ID's unchanged in any environment" $ do
            forAll arbitrary $ \env ->
                forAll arbitrary $ \s ->
                    replaceId env (Plain s) `shouldBe` Right s

        it "returns Left if a variable is not in the environment" $ do
            forAll arbitrary $ \var ->
                forAll (arbitrary `suchThat` (isNothing . lookup var)) $ \env ->
                    replaceId env (Var var) `shouldSatisfy` isLeft

        it "replaces a variable if it's in the environment" $ do
            forAll arbitrary $ \var ->
                forAll arbitrary $ \val ->
                    forAll (arbitrary `suchThat` (isNothing . lookup var)) $ \env1 ->
                        forAll (arbitrary `suchThat` (isNothing . lookup var)) $ \env2 ->
                            replaceId (env1 ++ [(var, val)] ++ env2) (Var var) `shouldBe` Right val


