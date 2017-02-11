{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Check.Gen where

import TestImport

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Language.Gen ()

instance GenUnchecked CheckAssignment

instance GenValid CheckAssignment

instance Arbitrary CheckAssignment where
    arbitrary = genValid

instance GenUnchecked CheckCardReference

instance GenValid CheckCardReference

instance Arbitrary CheckCardReference where
    arbitrary = genValid

instance GenUnchecked CheckSettings

instance GenValid CheckSettings

instance Arbitrary CheckSettings where
    arbitrary = genValid

instance GenUnchecked CheckError

instance GenValid CheckError

instance Arbitrary CheckError where
    arbitrary = genValid

instance GenUnchecked CheckResult

instance GenValid CheckResult

instance Arbitrary CheckResult where
    arbitrary = genValid

instance GenUnchecked Instruction

instance GenValid Instruction

instance Arbitrary Instruction where
    arbitrary = genValid

instance GenUnchecked CleanupInstruction

instance GenValid CleanupInstruction

instance Arbitrary CleanupInstruction where
    arbitrary = genValid

instance GenUnchecked DiagnosedDeployment

instance GenValid DiagnosedDeployment

instance Arbitrary DiagnosedDeployment where
    arbitrary = genValid

instance GenUnchecked Diagnostics

instance GenValid Diagnostics

instance Arbitrary Diagnostics where
    arbitrary = genValid

instance GenUnchecked DiagnosedFp

instance GenValid DiagnosedFp

instance Arbitrary DiagnosedFp where
    arbitrary = genValid

instance GenUnchecked DeploymentCheckResult

instance GenValid DeploymentCheckResult

instance Arbitrary DeploymentCheckResult where
    arbitrary = genValid

instance GenUnchecked MD5Digest where
    genUnchecked = md5 . LB.pack <$> arbitrary

instance Arbitrary MD5Digest where
    arbitrary = genUnchecked
