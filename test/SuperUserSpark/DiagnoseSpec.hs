{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.DiagnoseSpec where

import TestImport

import Control.Monad (forM_)
import Data.Hashable
import System.FilePath (dropTrailingPathSeparator)
import System.Posix.Files

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check
import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.TestUtils
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Diagnose
import SuperUserSpark.Diagnose.Gen ()
import SuperUserSpark.Diagnose.Internal
import SuperUserSpark.Diagnose.TestUtils
import SuperUserSpark.Diagnose.Types
import SuperUserSpark.OptParse.Gen ()
import SuperUserSpark.Parser.Gen ()
import SuperUserSpark.Utils
import TestUtils

spec :: Spec
spec = do
    instanceSpec
    diagnoseSpec
    hashSpec

instanceSpec :: Spec
instanceSpec = do
    eqSpec @CheckSettings
    genValidSpec @CheckSettings
    eqSpec @Diagnostics
    genValidSpec @Diagnostics
    eqSpec @DiagnosedFp
    genValidSpec @DiagnosedFp
    eqSpec @DiagnosedDeployment
    genValidSpec @DiagnosedDeployment

diagnoseSpec :: Spec
diagnoseSpec = do
    describe "formatDiagnoseError" $
        it "always produces valid strings" $ producesValid formatDiagnoseError
    sandbox <- runIO $ resolveDir' "test_sandbox"
    let setup = ensureDir sandbox
    let teardown = removeDirRecur sandbox
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "diagnoseDeployment" $ do
                it
                    "retains the filepaths and deploymentkind that it diagnoses for valid filepaths" $ do
                    forAll genValid $ \d@(Deployment (Directions srcs dst) kind) -> do
                        (Deployment (Directions dsrcs ddst) dkind) <-
                            diagnoseDeployment d
                        map diagnosedFilePath dsrcs `shouldBe` srcs
                        diagnosedFilePath ddst `shouldBe` dst
                        dkind `shouldBe` kind
                pend
            describe "diagnose" $ do
                it "retains the filepath that it diagnoses for valid AbsPath's" $ do
                    forAll genValid $ \fp -> do
                        (D dfp _ _) <- diagnoseAbsP fp
                        dfp `shouldBe` fp
                pend
            describe "diagnoseFp" $ do
                let expect s ls = do
                        rs <-
                            forM ls $ \(a, b) -> do
                                r <- diagnoseFp a
                                pure (a, r, b)
                        unless (all (\(a, r, b) -> r == b) rs) $
                            let nice (a, r, b) =
                                    unlines
                                        [ unwords
                                              [ "Unexpected results at stage"
                                              , show s
                                              ]
                                        , unwords ["path:", show $ toPath a]
                                        , unwords
                                              [ "stripped:"
                                              , show $
                                                (stripDir sandbox $ unAbsP a :: Maybe (Path Rel File))
                                              ]
                                        , unwords ["expected:", show b]
                                        , unwords ["real:", show r]
                                        ]
                            in expectationFailure $ unlines $ map nice rs
                it "figures out this link that points to something that exists" $ do
                    let file = sandbox </> $(mkRelFile "file")
                    let file' = AbsP file
                    let link = sandbox </> $(mkRelFile "link")
                    let link' = AbsP link
                    expect "before" [(file', Nonexistent), (link', Nonexistent)]
                    writeFile file "This is a test"
                    expect
                        "after file creation"
                        [(file', IsFile), (link', Nonexistent)]
                    createSymbolicLink (toFilePath file) (toFilePath link)
                    expect
                        "after link creation"
                        [(file', IsFile), (link', IsLinkTo file')]
                    removeLink $ toFilePath link
                    expect
                        "after link removal"
                        [(file', IsFile), (link', Nonexistent)]
                    removeFile file
                    expect
                        "after file removal"
                        [(file', Nonexistent), (link', Nonexistent)]
                it
                    "figures out this link that points to something that does not exist" $ do
                    let file = sandbox </> $(mkRelFile "file")
                    let file' = AbsP file
                    let link = sandbox </> $(mkRelFile "link")
                    let link' = AbsP link
                    expect "before" [(file', Nonexistent), (link', Nonexistent)]
                    createSymbolicLink (toFilePath file) (toFilePath link)
                    expect
                        "after link creation"
                        [(file', Nonexistent), (link', IsLinkTo file')]
                    removeLink $ toFilePath link
                    expect "after" [(file', Nonexistent), (link', Nonexistent)]
                it
                    "figures out that a thing in a nonexistent dir is nonexistent" $ do
                    let file = sandbox </> $(mkRelFile "nonexistent/and/file")
                    let file' = AbsP file
                    diagnoseFp file' `shouldReturn` Nonexistent
                it "figures out a file" $ do
                    forAll (absFileIn sandbox) $ \(file, file') -> do
                        diagnoseFp file' `shouldReturn` Nonexistent
                        ensureDir $ parent file
                        writeFile file "This is a test"
                        diagnoseFp file' `shouldReturn` IsFile
                        removeFile file
                        diagnoseFp file' `shouldReturn` Nonexistent
                it "figures out a directory" $ do
                    forAll (absDirIn sandbox) $ \(dir, dir') -> do
                        diagnoseFp dir' `shouldReturn` Nonexistent
                        ensureDir dir
                        diagnoseFp dir' `shouldReturn` IsDirectory
                        removeDirRecur dir
                        diagnoseFp dir' `shouldReturn` Nonexistent
                it "figures out a symbolic link with an existent destination" $ do
                    forAll (absFileIn sandbox) $ \f@(file, file') ->
                        forAll (absFileIn sandbox `suchThat` (/= f)) $ \(link, link') -> do
                            expect
                                "before"
                                [(file', Nonexistent), (link', Nonexistent)]
                            writeFile file "This is a test"
                            expect
                                "after file creation"
                                [(file', IsFile), (link', Nonexistent)]
                            createSymbolicLink
                                (toFilePath file)
                                (toFilePath link)
                            expect
                                "after link creation"
                                [(file', IsFile), (link', IsLinkTo file')]
                            removeLink $ toFilePath link
                            expect
                                "after link removal"
                                [(file', IsFile), (link', Nonexistent)]
                            removeFile file
                            expect
                                "after"
                                [(file', Nonexistent), (link', Nonexistent)]
                it "figures out a symbolic link with a nonexistent destination" $ do
                    forAll (absFileIn sandbox) $ \f@(file, file') ->
                        forAll (absFileIn sandbox `suchThat` (/= f)) $ \(link, link') -> do
                            expect
                                "before"
                                [(file', Nonexistent), (link', Nonexistent)]
                            createSymbolicLink
                                (toFilePath file)
                                (toFilePath link)
                            expect
                                "after link creation"
                                [(file', Nonexistent), (link', IsLinkTo file')]
                            removeLink $ toFilePath link
                            expect
                                "after"
                                [(file', Nonexistent), (link', Nonexistent)]
                it "figures out that /dev/null is weird" $ do
                    diagnoseFp (AbsP $(mkAbsFile "/dev/null")) `shouldReturn`
                        IsWeird
                it "figures out that /dev/random is weird" $ do
                    diagnoseFp (AbsP $(mkAbsFile "/dev/random")) `shouldReturn`
                        IsWeird

checkDeploymentSpec :: Spec
checkDeploymentSpec = do
    describe "checkDeployment" $ do
        it "always produces valid check results" $
            producesValidsOnValids checkDeployment
        it "says 'impossible' for deployments with an empty list of sources" $ do
            forAll genUnchecked $ \dst ->
                forAll genUnchecked $ \kind ->
                    shouldBeImpossible' $ Deployment (Directions [] dst) kind
        it "says 'impossible' for deployments where all singles are impossible" $ do
            forAll
                (genValid `suchThat`
                 (\(Deployment (Directions srcs dst) kind) ->
                      all (\src -> isImpossible $ checkSingle src dst kind) srcs)) $ \dd ->
                shouldBeImpossible' dd
        it
            "gives the same result as bestResult (just with a better error for empty lists)" $ do
            forAll genValid $ \dd@(Deployment (Directions srcs dst) kind) ->
                case ( bestResult (map (\src -> checkSingle src dst kind) srcs)
                     , checkDeployment dd) of
                    (ImpossibleDeployment r1, ImpossibleDeployment r2) ->
                        length r1 `shouldSatisfy` (<= (length r2))
                    (r1, r2) -> r1 `shouldBe` r2
    describe "bestResult" $ do
        it "always produces valid check results" $
            producesValidsOnValids bestResult
        it "says 'impossible' if all checkresults are impossible" $ do
            forAll
                (genValid `suchThat` all isImpossible)
                shouldBeImpossibleDeployment
        it "says 'done' if the first non-impossible in 'done'" $ do
            forAll
                (genValid `suchThat`
                 (any (not . isImpossible) &&&
                  (isDone . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` deploymentIsDone
        it "says 'dirty' if the first non-impossible in 'dirty'" $ do
            forAll
                (genValid `suchThat`
                 (any (not . isImpossible) &&&
                  (isDirty . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` dirtyDeployment
        it "says 'ready' if the first non-impossible in 'ready'" $ do
            forAll
                (genValid `suchThat`
                 (any (not . isImpossible) &&&
                  (isReady . head . dropWhile isImpossible))) $ \dd ->
                bestResult dd `shouldSatisfy` deploymentReadyToDeploy

hashSpec :: Spec
hashSpec = do
    tooManyFilesTest

tooManyFilesTest :: Spec
tooManyFilesTest = do
    sandbox <- runIO $ resolveDir' "test_sandbox"
    let setup = ensureDir sandbox
    let teardown = removeDirRecur sandbox
    let aLot = 20000 :: Int
    let setupALotOfFiles = do
            forM_ [1 .. aLot] $ \i -> do
                f <- parseRelFile $ "file" ++ show i
                writeFile (sandbox </> f) $ "This is file " ++ show i ++ ".\n"
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "hashFilePath" $ do
                beforeAll_ setupALotOfFiles $ do
                    it
                        ("has no problem with hashing a directory of " ++
                         show aLot ++ " files") $ do
                        sb <- resolveFile' "test_sandbox"
                        let d = AbsP sb
                        hashFilePath d `shouldNotReturn` HashDigest (hash ())
