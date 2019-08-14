{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Check.Gen where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Diagnose.Gen ()
import SuperUserSpark.Language.Gen ()

instance GenUnchecked CheckAssignment

instance GenValid CheckAssignment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked CheckSettings

instance GenValid CheckSettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked CheckResult

instance GenValid CheckResult where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked Instruction

instance GenValid Instruction where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked CleanupInstruction

instance GenValid CleanupInstruction where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked DeploymentCheckResult

instance GenValid DeploymentCheckResult where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
