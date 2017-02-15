{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Check.Gen where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Language.Gen ()

instance GenUnchecked CheckAssignment

instance GenValid CheckAssignment where
    genValid = CheckAssignment <$> genValid <*> genValid

instance Arbitrary CheckAssignment where
    arbitrary = genValid

instance GenUnchecked CheckSettings

instance GenValid CheckSettings where
    genValid = CheckSettings <$> genValid

instance Arbitrary CheckSettings where
    arbitrary = genValid

instance GenUnchecked CheckError

instance GenValid CheckError

instance GenUnchecked HashDigest

instance GenValid HashDigest

instance Arbitrary HashDigest where
    arbitrary = genValid

instance Arbitrary CheckError where
    arbitrary = genValid

instance GenUnchecked CheckResult where
    genUnchecked =
        oneof
            [ pure AlreadyDone
            , Ready <$> genUnchecked
            , Dirty <$> genUnchecked <*> genUnchecked <*> genUnchecked
            , Impossible <$> genUnchecked
            ]

instance GenValid CheckResult where
    genValid =
        oneof
            [ pure AlreadyDone
            , Ready <$> genValid
            , Dirty <$> genValid <*> genValid <*> genValid
            , Impossible <$> genValid
            ]

instance Arbitrary CheckResult where
    arbitrary = genValid

instance GenUnchecked Instruction

instance GenValid Instruction where
    genValid = Instruction <$> genValid <*> genValid <*> genValid

instance Arbitrary Instruction where
    arbitrary = genValid

instance GenUnchecked CleanupInstruction

instance GenValid CleanupInstruction

instance Arbitrary CleanupInstruction where
    arbitrary = genValid

instance GenUnchecked Diagnostics

instance GenValid Diagnostics

instance Arbitrary Diagnostics where
    arbitrary = genValid

instance GenUnchecked DiagnosedFp

instance GenValid DiagnosedFp where
    genValid = D <$> genValid <*> genValid <*> genValid

instance Arbitrary DiagnosedFp where
    arbitrary = genValid

instance GenUnchecked DeploymentCheckResult where
    genUnchecked =
        oneof
            [ pure DeploymentDone
            , ReadyToDeploy <$> genUnchecked
            , DirtySituation <$> genUnchecked <*> genUnchecked <*> genUnchecked
            , ImpossibleDeployment <$> genUnchecked
            ]

instance GenValid DeploymentCheckResult

instance Arbitrary DeploymentCheckResult where
    arbitrary = genValid
