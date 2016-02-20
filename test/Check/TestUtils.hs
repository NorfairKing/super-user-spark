module Check.TestUtils where

import           Check.Gen       ()
import           Check.Internal
import           Check.Types
import           CoreTypes
import           Test.Hspec
import           Test.QuickCheck

-- * Test utils for checkDeployment

isImpossibleDeployment :: DeploymentCheckResult -> Bool
isImpossibleDeployment (ImpossibleDeployment _) = True
isImpossibleDeployment _ = False

deploymentIsDone :: DeploymentCheckResult -> Bool
deploymentIsDone DeploymentDone = True
deploymentIsDone _ = False

dirtyDeployment :: DeploymentCheckResult -> Bool
dirtyDeployment (DirtySituation _ _) = True
dirtyDeployment _ = False

readyDeployment :: DeploymentCheckResult -> Bool
readyDeployment (ReadyToDeploy _) = True
readyDeployment _ = False

-- shouldBeImpossible :: [DiagnosedFp] -> DiagnosedFp -> CoreTypes.DeploymentKind -> Expectation
-- shouldBeImpossible srcs dst kind = checkDeployment (Diagnosed srcs dst kind) `shouldSatisfy` isImpossibleDeployment

shouldBeImpossible' :: DiagnosedDeployment -> Expectation
shouldBeImpossible' dd = checkDeployment dd `shouldSatisfy` isImpossibleDeployment

shouldBeImpossibleDeployment :: [CheckResult] -> Expectation
shouldBeImpossibleDeployment dd = bestResult dd `shouldSatisfy` isImpossibleDeployment


-- * Test utils for checkSingle

isDirty :: CheckResult -> Bool
isDirty (Dirty _ _) = True
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

shouldBeDirty :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeDirty src dst kind = checkSingle src dst kind `shouldSatisfy` isDirty

shouldBeReady :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeReady src dst kind = checkSingle src dst kind `shouldSatisfy` isReady

shouldBeDone :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeDone src dst kind = checkSingle src dst kind `shouldSatisfy` isDone

shouldBeImpossible ::  DiagnosedFp -> DiagnosedFp -> DeploymentKind -> Expectation
shouldBeImpossible src dst kind = checkSingle src dst kind `shouldSatisfy` isImpossible


arbitraryWith :: Diagnostics -> Gen DiagnosedFp
arbitraryWith d = D <$> arbitrary <*> pure d <*> arbitrary

