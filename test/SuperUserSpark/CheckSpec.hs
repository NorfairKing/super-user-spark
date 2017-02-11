{-# LANGUAGE TypeApplications #-}

module SuperUserSpark.CheckSpec where

import TestImport

import Control.Monad (forM_)
import qualified Data.ByteString as SB
import SuperUserSpark.Check
import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.TestUtils
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.OptParse.Gen ()
import SuperUserSpark.Parser.Gen
import SuperUserSpark.Utils
import System.Directory
       (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Posix.Files
import TestUtils

spec :: Spec
spec = do
    instanceSpec
    checkSpec
    diagnoseSpec
    hashSpec

instanceSpec :: Spec
instanceSpec = do
    eqSpec @CheckCardReference
    genValidSpec @CheckCardReference
    eqSpec @CheckSettings
    genValidSpec @CheckSettings
    eqSpec @Diagnostics
    genValidSpec @Diagnostics
    eqSpec @DiagnosedFp
    genValidSpec @DiagnosedFp
    eqSpec @Instruction
    genValidSpec @Instruction
    eqSpec @CleanupInstruction
    genValidSpec @CleanupInstruction
    eqSpec @DeploymentCheckResult
    genValidSpec @DeploymentCheckResult
    eqSpec @CheckResult
    genValidSpec @CheckResult
    eqSpec @DiagnosedDeployment
    genValidSpec @DiagnosedDeployment

checkSpec :: Spec
checkSpec =
    parallel $ do
        describe "checkAssignment" $
            it "always produces valid assignments" $
            validIfSucceeds checkAssignment
        describe "deriveCheckSettings" $
            it "always produces valid settings" $
            validIfSucceeds deriveCheckSettings
        describe "formatCheckError" $
            it "always produces valid strings" $ producesValid formatCheckError
        checkSingleSpec
        checkDeploymentSpec
        describe "formatDeploymentChecks" $
            it "always produces valid strings" $
            producesValid formatDeploymentChecks
        describe "formatDeploymentCheck" $
            it "always produces valid strings" $
            producesValid formatDeploymentCheck
        describe "formatInstruction" $
            it "always produces valid strings" $ producesValid formatInstruction
        describe "formatCleanupInstruction" $
            it "always produces valid strings" $
            producesValid formatCleanupInstruction

diagnoseSpec :: Spec
diagnoseSpec = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing sandbox
    let teardown = removeDirectoryRecursive sandbox
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "diagnoseDeployment" $ do
                it
                    "retains the filepaths and deploymentkind that it diagnoses for very simple filepaths" $ do
                    once $
                        forAll (resize 5 $ listOf generateWord) $ \srcs -> do
                            forAll generateWord $ \dst -> do
                                forAll arbitrary $ \kind -> do
                                    (Diagnosed dsrcs ddst dkind) <-
                                        diagnoseDeployment $ Put srcs dst kind
                                    map diagnosedFilePath dsrcs `shouldBe` srcs
                                    diagnosedFilePath ddst `shouldBe` dst
                                    dkind `shouldBe` kind
                pend
            describe "diagnose" $ do
                it
                    "retains the filepath that it diagnoses for very simple filepaths" $ do
                    once $
                        forAll generateWord $ \fp -> do
                            (D dfp _ _) <- diagnose fp
                            dfp `shouldBe` fp
                pend
            describe "diagnoseFp" $ do
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
                        createDirectoryIfMissing dir
                        diagnoseFp dir `shouldReturn` IsDirectory
                        removeDirectoryRecursive dir
                        diagnoseFp dir `shouldReturn` Nonexistent
                it "figures out this test symbolic link with a destination" $ do
                    withCurrentDirectory sandbox $ do
                        let link = "testlink"
                        let file = "testfile"
                        writeFile file "This is a test"
                        createSymbolicLink file link
                        diagnoseFp file `shouldReturn` IsFile
                        diagnoseFp link `shouldReturn` IsLinkTo file
                        removeLink link
                        removeFile file
                        diagnoseFp link `shouldReturn` Nonexistent
                it "figures out this test symbolic link without a destination" $ do
                    withCurrentDirectory sandbox $ do
                        let link = "testlink"
                        let file = "testfile"
                        createSymbolicLink file link
                        diagnoseFp file `shouldReturn` Nonexistent
                        diagnoseFp link `shouldReturn` IsLinkTo file
                        removeLink link
                        diagnoseFp file `shouldReturn` Nonexistent
                        diagnoseFp link `shouldReturn` Nonexistent
                it "figures out that /dev/null is weird" $ do
                    diagnoseFp "/dev/null" `shouldReturn` IsWeird
                it "figures out that /dev/random is weird" $ do
                    diagnoseFp "/dev/random" `shouldReturn` IsWeird

checkDeploymentSpec :: Spec
checkDeploymentSpec = do
    describe "checkDeployment" $ do
        it "always produces valid check results" $ producesValid checkDeployment
        it "says 'impossible' for deployments with an empty list of sources" $ do
            forAll arbitrary $ \dst ->
                forAll arbitrary $ \kind ->
                    shouldBeImpossible' $ Diagnosed [] dst kind
        it "says 'impossible' for deployments where all singles are impossible" $ do
            forAll
                (arbitrary `suchThat`
                 (\(Diagnosed srcs dst kind) ->
                      all (\src -> isImpossible $ checkSingle src dst kind) srcs)) $ \dd ->
                shouldBeImpossible' dd
        it
            "gives the same result as bestResult (just with a better error for empty lists)" $ do
            property $ \dd@(Diagnosed srcs dst kind) ->
                case ( bestResult (map (\src -> checkSingle src dst kind) srcs)
                     , checkDeployment dd) of
                    (ImpossibleDeployment r1, ImpossibleDeployment r2) ->
                        length r1 `shouldSatisfy` (<= (length r2))
                    (r1, r2) -> r1 `shouldBe` r2
    describe "bestResult" $ do
        it "always produces valid check results" $ producesValid bestResult
        it "says 'impossible' if all checkresults are impossible" $ do
            forAll
                (arbitrary `suchThat` all isImpossible)
                shouldBeImpossibleDeployment
        it "says 'done' if the first non-impossible in 'done'" $ do
            forAll
                (arbitrary `suchThat`
                 (any (not . isImpossible) &&&
                  (isDone . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` deploymentIsDone
        it "says 'dirty' if the first non-impossible in 'dirty'" $ do
            forAll
                (arbitrary `suchThat`
                 (any (not . isImpossible) &&&
                  (isDirty . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` dirtyDeployment
        it "says 'ready' if the first non-impossible in 'ready'" $ do
            forAll
                (arbitrary `suchThat`
                 (any (not . isImpossible) &&&
                  (isReady . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` deploymentReadyToDeploy

checkSingleSpec :: Spec
checkSingleSpec =
    describe "checkSingle" $ do
        it "always produces valid CheckResults" $ producesValid3 checkSingle
        it "says 'impossible' if the source does not exist" $ do
            forAll (arbitraryWith Nonexistent) $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \kind -> shouldBeImpossible src dst kind
        it
            "says 'ready' if the source is a file and the destination does not exist" $ do
            forAll (arbitraryWith IsFile) $ \src ->
                forAll (arbitraryWith Nonexistent) $ \dst ->
                    forAll arbitrary $ \kind -> shouldBeReady src dst kind
        it
            "says 'dirty' if both the source and destination are files and it's a link deployment" $ do
            forAll (arbitraryWith IsFile) $ \src ->
                forAll (arbitraryWith IsFile) $ \dst ->
                    shouldBeDirty src dst LinkDeployment $
                    CleanFile $ diagnosedFilePath dst
        it
            "says 'done' if both the source and destination are files and it's a copy deployment and the files are equal" $ do
            forAll arbitrary $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \h1 ->
                        shouldBeDone
                            (D src IsFile h1)
                            (D dst IsFile h1)
                            CopyDeployment
        it
            "says 'dirty' if both the source and destination are files and it's a copy deployment but the files are unequal" $ do
            forAll arbitrary $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \h1 ->
                        forAll (arbitrary `suchThat` (/= h1)) $ \h2 ->
                            shouldBeDirty
                                (D src IsFile h1)
                                (D dst IsFile h2)
                                CopyDeployment $
                            CleanFile dst
        it
            "says 'dirty' if the source is a file and the destination is a directory" $ do
            forAll (arbitraryWith IsFile) $ \src ->
                forAll (arbitraryWith IsDirectory) $ \dst ->
                    forAll arbitrary $ \kind ->
                        shouldBeDirty src dst kind $
                        CleanDirectory $ diagnosedFilePath dst
        it
            "says 'dirty' if the source is a file and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
            forAll (arbitraryWith IsFile) $ \src@(D srcp _ _) ->
                forAll (arbitrary `suchThat` (/= srcp)) $ \l ->
                    forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst LinkDeployment $
                        CleanLink $ diagnosedFilePath dst
        it
            "says 'done' if the source is a file and the destination is a link for a link deployment and the destination points to the source" $ do
            forAll (arbitraryWith IsFile) $ \src@(D srcp _ _) ->
                forAll (arbitraryWith $ IsLinkTo srcp) $ \dst ->
                    shouldBeDone src dst LinkDeployment
        it
            "says 'dirty' if the source is a file and the destination is a link for a copy deployment" $ do
            forAll (arbitraryWith IsFile) $ \src ->
                forAll arbitrary $ \l ->
                    forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst CopyDeployment $
                        CleanLink $ diagnosedFilePath dst
        it
            "says 'ready' if the source is a directory and the destination does not exist" $ do
            forAll (arbitraryWith IsDirectory) $ \src ->
                forAll (arbitraryWith Nonexistent) $ \dst ->
                    forAll arbitrary $ \kind -> shouldBeReady src dst kind
        it
            "says 'dirty' if the source is a directory and the destination is a file" $ do
            forAll (arbitraryWith IsDirectory) $ \src ->
                forAll (arbitraryWith IsFile) $ \dst ->
                    forAll arbitrary $ \kind ->
                        shouldBeDirty src dst kind $
                        CleanFile $ diagnosedFilePath dst
        it
            "says 'dirty' if both the source and destination are directories for a link deployment" $ do
            forAll (arbitraryWith IsDirectory) $ \src ->
                forAll (arbitraryWith IsDirectory) $ \dst ->
                    shouldBeDirty src dst LinkDeployment $
                    CleanDirectory $ diagnosedFilePath dst
        it
            "says 'done' if both the source and destination are directories and it's a copy deployment and the directories are equal" $ do
            forAll arbitrary $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \h1 ->
                        shouldBeDone
                            (D src IsDirectory h1)
                            (D dst IsDirectory h1)
                            CopyDeployment
        it
            "says 'dirty' if both the source and destination are directories and it's a copy deployment but the directories are unequal" $ do
            forAll arbitrary $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \h1 ->
                        forAll (arbitrary `suchThat` (/= h1)) $ \h2 ->
                            shouldBeDirty
                                (D src IsDirectory h1)
                                (D dst IsDirectory h2)
                                CopyDeployment $
                            CleanDirectory dst
        it
            "says 'dirty' if the source is a directory and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
            forAll (arbitraryWith IsDirectory) $ \src@(D srcp _ _) ->
                forAll (arbitrary `suchThat` (/= srcp)) $ \l ->
                    forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst LinkDeployment $
                        CleanLink $ diagnosedFilePath dst
        it
            "says 'done' if the source is a directory and the destination is a link for a link deployment and the destination points to the source" $ do
            forAll (arbitraryWith IsDirectory) $ \src@(D srcp _ _) ->
                forAll (arbitraryWith $ IsLinkTo srcp) $ \dst ->
                    shouldBeDone src dst LinkDeployment
        it
            "says 'dirty' if the source is a directory and the destination is a link for a copy deployment" $ do
            forAll (arbitraryWith IsDirectory) $ \src ->
                forAll arbitrary $ \l ->
                    forAll (arbitraryWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst CopyDeployment $
                        CleanLink $ diagnosedFilePath dst
        it "says 'dirty' if the source is a link" $ do
            forAll arbitrary $ \l ->
                forAll (arbitraryWith $ IsLinkTo l) $ \src ->
                    forAll arbitrary $ \dst ->
                        forAll arbitrary $ \kind ->
                            shouldBeImpossible src dst kind
        it "says 'dirty' for a weird source" $ do
            forAll (arbitraryWith IsWeird) $ \src ->
                forAll arbitrary $ \dst ->
                    forAll arbitrary $ \kind -> shouldBeImpossible src dst kind
        it "says 'dirty' for a weird destination" $ do
            forAll arbitrary $ \src ->
                forAll (arbitraryWith IsWeird) $ \dst ->
                    forAll arbitrary $ \kind -> shouldBeImpossible src dst kind
        it "works for these unit tests" $ do pending

hashSpec :: Spec
hashSpec = do
    tooManyFilesTest

tooManyFilesTest :: Spec
tooManyFilesTest = do
    let sandbox = "test_sandbox"
    let setup = createDirectoryIfMissing sandbox
    let teardown = removeDirectoryRecursive sandbox
    let aLot = 20000 :: Int
    let setupALotOfFiles = do
            forM_ [1 .. aLot] $ \i ->
                writeFile (sandbox ++ "/file" ++ show i) $
                "This is file " ++ show i ++ ".\n"
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "hashFilePath" $ do
                beforeAll_ setupALotOfFiles $ do
                    it
                        ("has no problem with hashing a directory of " ++
                         show aLot ++ " files") $ do
                        hashFilePath "test_sandbox" `shouldNotReturn`
                            md5 SB.empty
