module CheckSpec where

import           Check
import           Check.Gen          ()
import           Check.Internal
import           Check.Types
import           CoreTypes
import           System.Directory
import           System.Posix.Files
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    diagnoseSpec
    checkSpec

diagnoseSpec :: Spec
diagnoseSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing True sandbox
    let teardown = removeDirectoryRecursive sandbox
    beforeAll_ setup $ afterAll_ teardown $ describe "diagnoseFp" $ do
        it "figures out this test file" $ do
            withCurrentDirectory sandbox $ do
                let file = "test.txt"
                writeFile file "This is a test"
                diagnoseFp file `shouldReturn` IsFile
                removeFile file
                diagnoseFp file `shouldReturn` Nonexistent

        it "figures out this test directory" $ do
            withCurrentDirectory sandbox $ do
                let dir = "testdir"
                createDirectory dir
                diagnoseFp dir `shouldReturn` IsDirectory
                removeDirectory dir
                diagnoseFp dir `shouldReturn` Nonexistent

        it "figures out this test symbolic link with a destination" $ do
            withCurrentDirectory sandbox $ do
                let link = "testlink"
                let file = "testfile"
                writeFile file "This is a test"
                createSymbolicLink file link
                diagnoseFp link `shouldReturn` IsLinkTo file
                removeLink link
                removeFile file
                diagnoseFp link `shouldReturn` Nonexistent

        it "figures out this test symbolic link without a destination" $ do
            withCurrentDirectory sandbox $ do
                let link = "testlink"
                let file = "testfile"
                createSymbolicLink file link
                diagnoseFp link `shouldReturn` IsLinkTo file
                removeLink link
                diagnoseFp link `shouldReturn` Nonexistent

        it "figures out that /dev/null is weird" $ do
            diagnoseFp "/dev/null" `shouldReturn` IsWeird

        it "figures out that /dev/random is weird" $ do
            diagnoseFp "/dev/random" `shouldReturn` IsWeird

checkSpec :: Spec
checkSpec = parallel $ do
    checkSingleSpec

checkSingleSpec :: Spec
checkSingleSpec = describe "checkSingle" $ do

    let isDirty (Dirty _) = True
        isDirty _ = False
    let isReady Ready = True
        isReady _ = False
    let isDone AlreadyDone = True
        isDone _ = False

    let shouldBeDirty src dst kind = checkSingle src dst kind `shouldSatisfy` isDirty
    let shouldBeReady src dst kind = checkSingle src dst kind `shouldSatisfy` isReady
    let shouldBeDone src dst kind = checkSingle src dst kind `shouldSatisfy` isDone

    let arbitraryWith :: Diagnostics -> Gen DiagnosedFp
        arbitraryWith d = D <$> arbitrary <*> pure d <*> arbitrary

    it "says 'error' if the source does not exist" $ do
        forAll (arbitraryWith Nonexistent) $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeDirty src dst kind

    it "says 'ready' if the source is a file and the destination does not exist" $ do
        forAll (arbitraryWith IsFile) $ \src ->
            forAll (arbitraryWith Nonexistent) $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeReady src dst kind

    it "says 'error' if both the source and destination are files and it's a link deployment" $ do
        forAll (arbitraryWith IsFile) $ \src ->
            forAll (arbitraryWith IsFile) $ \dst ->
                shouldBeDirty src dst LinkDeployment

    it "says 'done' if both the source and destination are files and it's a copy deployment and the files are equal" $ do
        forAll arbitrary $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \h1 ->
                        shouldBeDone (D src IsFile h1) (D dst IsFile h1) CopyDeployment

    it "says 'error' if both the source and destination are files and it's a copy deployment but the files are unequal" $ do
        forAll arbitrary $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \h1 ->
                    forAll (arbitrary `suchThat` (/= h1)) $ \h2 ->
                        shouldBeDirty (D src IsFile h1) (D dst IsFile h2) CopyDeployment

    it "says 'error' if the source is a file and the destination is a directory" $ do
        forAll (arbitraryWith IsFile) $ \src ->
            forAll (arbitraryWith IsDirectory) $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeDirty src dst kind

    it "says 'error' if the source is a file and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
        forAll (arbitraryWith IsFile) $ \src@(D srcp _ _) ->
            forAll (arbitrary `suchThat` (/= srcp)) $ \l ->
                forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                    shouldBeDirty src dst LinkDeployment

    it "says 'done' if the source is a file and the destination is a link for a link deployment and the destination points to the source" $ do
        forAll (arbitraryWith IsFile) $ \src@(D srcp _ _) ->
            forAll (arbitraryWith $ IsLinkTo srcp) $ \dst ->
                shouldBeDone src dst LinkDeployment

    it "says 'error' if the source is a file and the destination is a link for a copy deployment" $ do
        forAll (arbitraryWith IsFile)  $ \src ->
            forAll arbitrary $ \l ->
                forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                    shouldBeDirty src dst CopyDeployment

    it "says 'ready' if the source is a directory and the destination does not exist" $ do
        forAll (arbitraryWith IsDirectory) $ \src ->
            forAll (arbitraryWith Nonexistent) $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeReady src dst kind

    it "says 'error' if the source is a directory and the destination is a file" $ do
        forAll (arbitraryWith IsDirectory) $ \src ->
            forAll (arbitraryWith IsFile) $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeDirty src dst kind

    it "says 'error' if both the source and destination are directories for a link deployment" $ do
        forAll (arbitraryWith IsDirectory) $ \src ->
            forAll (arbitraryWith IsDirectory) $ \dst ->
                shouldBeDirty src dst LinkDeployment

    it "says 'done' if both the source and destination are directories and it's a copy deployment and the directories are equal" $ do
        forAll arbitrary $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \h1 ->
                        shouldBeDone (D src IsDirectory h1) (D dst IsDirectory h1) CopyDeployment

    it "says 'error' if both the source and destination are directories and it's a copy deployment but the directories are unequal" $ do
        forAll arbitrary $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \h1 ->
                    forAll (arbitrary `suchThat` (/= h1)) $ \h2 ->
                        shouldBeDirty (D src IsDirectory h1) (D dst IsDirectory h2) CopyDeployment

    it "says 'error' if the source is a directory and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
        forAll (arbitraryWith IsDirectory) $ \src@(D srcp _ _) ->
            forAll (arbitrary `suchThat` (/= srcp)) $ \l ->
                forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                    shouldBeDirty src dst LinkDeployment

    it "says 'done' if the source is a directory and the destination is a link for a link deployment and the destination points to the source" $ do
        forAll (arbitraryWith IsDirectory) $ \src@(D srcp _ _) ->
            forAll (arbitraryWith $ IsLinkTo srcp) $ \dst ->
                shouldBeDone src dst LinkDeployment

    it "says 'done' if the source is a directory and the destination is a link for a copy deployment" $ do
        forAll (arbitraryWith IsDirectory) $ \src ->
            forAll arbitrary $ \l ->
                forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                    shouldBeDirty src dst CopyDeployment

    it "says 'error' if the source is a link" $ do
        forAll arbitrary $ \l ->
            forAll (arbitraryWith $ IsLinkTo l) $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \kind ->
                        shouldBeDirty src dst kind


    it "says 'error' for a weird source" $ do
        forAll (arbitraryWith IsWeird) $ \src ->
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeDirty src dst kind

    it "says 'error' for a weird destination" $ do
        forAll arbitrary $ \src ->
            forAll (arbitraryWith IsWeird) $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeDirty src dst kind

    it "works for these unit tests" $ do
        pending


