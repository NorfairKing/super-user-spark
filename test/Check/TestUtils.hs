module Check.TestUtils where

import           Check
import           Check.Gen       ()
import           Check.Types
import           CoreTypes
import           Test.Hspec
import           Test.QuickCheck

-- * Test utils for checkDeployment

isImpossible :: DeploymentCheckResult -> Bool
isImpossible (Impossible _) = True
isImpossible _ = False

shouldBeImpossible :: [DiagnosedFp] -> DiagnosedFp -> CoreTypes.DeploymentKind -> Expectation
shouldBeImpossible srcs dst kind = checkDeployment (Diagnosed srcs dst kind) `shouldSatisfy` isImpossible


-- * Test utils for checkSingle
isDirty :: CheckResult -> Bool
isDirty (Dirty _) = True
isDirty _ = False

isReady :: CheckResult -> Bool
isReady Ready = True
isReady _ = False

isDone :: CheckResult -> Bool
isDone AlreadyDone = True
isDone _ = False

shouldBeDirty :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeDirty src dst kind = checkSingle src dst kind `shouldSatisfy` isDirty

shouldBeReady :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeReady src dst kind = checkSingle src dst kind `shouldSatisfy` isReady

shouldBeDone :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeDone src dst kind = checkSingle src dst kind `shouldSatisfy` isDone

arbitraryWith :: Diagnostics -> Gen DiagnosedFp
arbitraryWith d = D <$> arbitrary <*> pure d <*> arbitrary

