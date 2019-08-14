{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.CheckSpec where

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
    checkSpec

instanceSpec :: Spec
instanceSpec = do
    eqSpec @CheckSettings
    genValidSpec @CheckSettings
    eqSpec @Instruction
    genValidSpec @Instruction
    eqSpec @CleanupInstruction
    genValidSpec @CleanupInstruction
    eqSpec @DeploymentCheckResult
    genValidSpec @DeploymentCheckResult
    eqSpec @CheckResult
    genValidSpec @CheckResult

checkSpec :: Spec
checkSpec =
    parallel $ do
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

checkSingleSpec :: Spec
checkSingleSpec =
    describe "checkSingle" $ do
        it "always produces valid CheckResults" $
            producesValidsOnValids3 checkSingle
        it "says 'impossible' if the source does not exist" $ do
            forAll (validWith Nonexistent) $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \kind -> shouldBeImpossible src dst kind
        it
            "says 'ready' if the source is a file and the destination does not exist" $ do
            forAll (validWith IsFile) $ \src ->
                forAll (validWith Nonexistent) $ \dst ->
                    forAll genValid $ \kind -> shouldBeReady src dst kind
        it
            "says 'dirty' if both the source and destination are files and it's a link deployment" $ do
            forAll (validWith IsFile) $ \src ->
                forAll (validWith IsFile) $ \dst ->
                    shouldBeDirty src dst LinkDeployment $
                    CleanFile $ unAbsP $ diagnosedFilePath dst
        it
            "says 'done' if both the source and destination are files and it's a copy deployment and the files are equal" $ do
            forAll genValid $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \h1 ->
                        shouldBeDone
                            (D src IsFile h1)
                            (D dst IsFile h1)
                            CopyDeployment
        it
            "says 'dirty' if both the source and destination are files and it's a copy deployment but the files are unequal" $ do
            forAll genValid $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \h1 ->
                        forAll (genValid `suchThat` (/= h1)) $ \h2 ->
                            shouldBeDirty
                                (D src IsFile h1)
                                (D dst IsFile h2)
                                CopyDeployment $
                            CleanFile $ unAbsP dst
        it
            "says 'dirty' if the source is a file and the destination is a directory" $ do
            forAll (validWith IsFile) $ \src ->
                forAll (validWith IsDirectory) $ \dst ->
                    forAll genValid $ \kind -> do
                        d <- parseAbsDir (toPath $ diagnosedFilePath dst)
                        shouldBeDirty src dst kind $ CleanDirectory d
        it
            "says 'dirty' if the source is a file and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
            forAll (validWith IsFile) $ \src@(D srcp _ _) ->
                forAll (genValid `suchThat` (/= srcp)) $ \l ->
                    forAll (validWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst LinkDeployment $
                        CleanLink $ unAbsP $ diagnosedFilePath dst
        it
            "says 'done' if the source is a file and the destination is a link for a link deployment and the destination points to the source" $ do
            forAll (validWith IsFile) $ \src@(D srcp _ _) ->
                forAll (validWith $ IsLinkTo srcp) $ \dst ->
                    shouldBeDone src dst LinkDeployment
        it
            "says 'dirty' if the source is a file and the destination is a link for a copy deployment" $ do
            forAll (validWith IsFile) $ \src ->
                forAll genValid $ \l ->
                    forAll (validWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst CopyDeployment $
                        CleanLink $ unAbsP $ diagnosedFilePath dst
        it
            "says 'ready' if the source is a directory and the destination does not exist" $ do
            forAll (validWith IsDirectory) $ \src ->
                forAll (validWith Nonexistent) $ \dst ->
                    forAll genValid $ \kind -> shouldBeReady src dst kind
        it
            "says 'dirty' if the source is a directory and the destination is a file" $ do
            forAll (validWith IsDirectory) $ \src ->
                forAll (validWith IsFile) $ \dst ->
                    forAll genValid $ \kind ->
                        shouldBeDirty src dst kind $
                        CleanFile $ unAbsP $ diagnosedFilePath dst
        it
            "says 'dirty' if both the source and destination are directories for a link deployment" $ do
            forAll (validWith IsDirectory) $ \src ->
                forAll (validWith IsDirectory) $ \dst -> do
                    d <- parseAbsDir (toPath $ diagnosedFilePath dst)
                    shouldBeDirty src dst LinkDeployment $ CleanDirectory d
        it
            "says 'done' if both the source and destination are directories and it's a copy deployment and the directories are equal" $ do
            forAll genValid $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \h1 ->
                        shouldBeDone
                            (D src IsDirectory h1)
                            (D dst IsDirectory h1)
                            CopyDeployment
        it
            "says 'dirty' if both the source and destination are directories and it's a copy deployment but the directories are unequal" $ do
            forAll genValid $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \h1 ->
                        forAll (genValid `suchThat` (/= h1)) $ \h2 -> do
                            d <- parseAbsDir $ toPath dst
                            shouldBeDirty
                                (D src IsDirectory h1)
                                (D dst IsDirectory h2)
                                CopyDeployment $
                                CleanDirectory d
        it
            "says 'dirty' if the source is a directory and the destination is a link for a link deployment but the destination doesn't point to the source" $ do
            forAll (validWith IsDirectory) $ \src@(D srcp _ _) ->
                forAll (genValid `suchThat` (/= srcp)) $ \l ->
                    forAll (validWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst LinkDeployment $
                        CleanLink $ unAbsP $ diagnosedFilePath dst
        it
            "says 'done' if the source is a directory and the destination is a link for a link deployment and the destination points to the source" $ do
            forAll (validWith IsDirectory) $ \src@(D srcp _ _) ->
                forAll (validWith $ IsLinkTo srcp) $ \dst ->
                    shouldBeDone src dst LinkDeployment
        it
            "says 'dirty' if the source is a directory and the destination is a link for a copy deployment" $ do
            forAll (validWith IsDirectory) $ \src ->
                forAll genValid $ \l ->
                    forAll (validWith $ IsLinkTo l) $ \dst ->
                        shouldBeDirty src dst CopyDeployment $
                        CleanLink $ unAbsP $ diagnosedFilePath dst
        it "says 'dirty' if the source is a link" $ do
            forAll genValid $ \l ->
                forAll (validWith $ IsLinkTo l) $ \src ->
                    forAll genValid $ \dst ->
                        forAll genValid $ \kind ->
                            shouldBeImpossible src dst kind
        it "says 'dirty' for a weird source" $ do
            forAll (validWith IsWeird) $ \src ->
                forAll genValid $ \dst ->
                    forAll genValid $ \kind -> shouldBeImpossible src dst kind
        it "says 'dirty' for a weird destination" $ do
            forAll genValid $ \src ->
                forAll (validWith IsWeird) $ \dst ->
                    forAll genValid $ \kind -> shouldBeImpossible src dst kind
        it "works for these unit tests" $ do pending
