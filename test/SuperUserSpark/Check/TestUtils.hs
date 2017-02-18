{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.Check.TestUtils where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.CoreTypes

-- * Test utils for checkDeployment
shouldBeImpossible' :: DiagnosedDeployment -> Expectation
shouldBeImpossible' dd = checkDeployment dd `shouldSatisfy` impossibleDeployment

shouldBeImpossibleDeployment :: [CheckResult] -> Expectation
shouldBeImpossibleDeployment dd =
    bestResult dd `shouldSatisfy` impossibleDeployment

-- * Test utils for checkSingle
isDirty :: CheckResult -> Bool
isDirty (Dirty _ _ _) = True
isDirty _ = False

isReady :: CheckResult -> Bool
isReady (Ready _) = True
isReady _ = False

isDone :: CheckResult -> Bool
isDone AlreadyDone = True
isDone _ = False

isImpossible :: CheckResult -> Bool
isImpossible (Impossible _) = True
isImpossible _ = False

shouldBeDirty
    :: DiagnosedFp
    -> DiagnosedFp
    -> DeploymentKind
    -> CleanupInstruction
    -> Expectation
shouldBeDirty src dst kind eci =
    case checkSingle src dst kind of
        Dirty _ (Instruction isrc idst ikind) ci -> do
            isrc `shouldBe` diagnosedFilePath src
            idst `shouldBe` diagnosedFilePath dst
            ikind `shouldBe` kind
            ci `shouldBe` eci
        t ->
            expectationFailure $
            unlines
                [ "checkSingle"
                , show src
                , show dst
                , show kind
                , "should be dirty but is"
                , show t
                ]

shouldBeReady :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeReady src dst kind = checkSingle src dst kind `shouldSatisfy` isReady

shouldBeDone :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeDone src dst kind = checkSingle src dst kind `shouldSatisfy` isDone

shouldBeImpossible :: DiagnosedFp
                   -> DiagnosedFp
                   -> DeploymentKind
                   -> Expectation
shouldBeImpossible src dst kind =
    checkSingle src dst kind `shouldSatisfy` isImpossible

validWith :: Diagnostics -> Gen DiagnosedFp
validWith d = D <$> genValid <*> pure d <*> genValid

-- TODO, the code should be able to handle 'genValid', but currenty it can't.
absPathIn :: Path Abs Dir -> Gen AbsP
absPathIn sandbox =
    scale (+ 5) $ do
        fp <- genListOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
        case parseRelFile fp of
            Nothing -> absPathIn sandbox
            Just p -> pure $ AbsP $ sandbox </> p

absFileIn :: Path Abs Dir -> Gen (Path Abs File, AbsP)
absFileIn sandbox = do
    p <- absPathIn sandbox
    pure (unAbsP p, p)

absDirIn :: Path Abs Dir -> Gen (Path Abs Dir, AbsP)
absDirIn sandbox = do
    p <- absPathIn sandbox
    let u = toPath p
    case parseAbsDir u of
        Nothing -> absDirIn sandbox
        Just ad -> pure (ad, p)
