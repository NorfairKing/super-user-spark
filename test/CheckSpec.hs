module CheckSpec where

import           Check
import           System.Directory
import           System.FilePath       (dropFileName, normalise, (</>))
import           System.FilePath.Posix (isAbsolute)
import           System.Posix.Files
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils

spec :: Spec
spec = parallel $ do
    diagnoseSpec

diagnoseSpec :: Spec
diagnoseSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing True sandbox
    let teardown = removeDirectoryRecursive sandbox
    beforeAll_ setup $ afterAll_ teardown $ describe "diagnose" $ do
        it "figures out this test file" $ do
            withCurrentDirectory sandbox $ do
                let file = "test.txt"
                writeFile file "This is a test"
                diagnose file `shouldReturn` IsFile
                removeFile file
                diagnose file `shouldReturn` Nonexistent
        it "figures out this test directory" $ do
            withCurrentDirectory sandbox $ do
                let dir = "testdir"
                createDirectory dir
                diagnose dir `shouldReturn` IsDirectory
                removeDirectory dir
                diagnose dir `shouldReturn` Nonexistent
        it "figures out this test symbolic link with a destination" $ do
            withCurrentDirectory sandbox $ do
                let link = "testlink"
                let file = "testfile"
                writeFile file "This is a test"
                createSymbolicLink file link
                diagnose link `shouldReturn` IsLink
                removeLink link
                removeFile file
                diagnose link `shouldReturn` Nonexistent
        it "figures out this test symbolic link without a destination" $ do
            withCurrentDirectory sandbox $ do
                let link = "testlink"
                let file = "testfile"
                createSymbolicLink file link
                diagnose link `shouldReturn` IsLink
                removeLink link
                diagnose link `shouldReturn` Nonexistent
        it "figures out that /dev/null is weird" $ do
            diagnose "/dev/null" `shouldReturn` IsWeird
        it "figures out that /dev/random is weird" $ do
            diagnose "/dev/random" `shouldReturn` IsWeird

