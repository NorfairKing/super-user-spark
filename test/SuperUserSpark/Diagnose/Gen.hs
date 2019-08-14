{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Diagnose.Gen where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Diagnose.Types
import SuperUserSpark.Language.Gen ()

instance GenUnchecked DiagnoseAssignment

instance GenValid DiagnoseAssignment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked DiagnoseSettings

instance GenValid DiagnoseSettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked HashDigest

instance GenValid HashDigest where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked Diagnostics

instance GenValid Diagnostics where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked DiagnosedFp

instance GenValid DiagnosedFp where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
