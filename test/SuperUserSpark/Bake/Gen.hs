{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Bake.Gen where

import TestImport

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Gen ()

instance GenUnchecked BakeAssignment

instance GenValid BakeAssignment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked BakeCardReference

instance GenValid BakeCardReference where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked BakeSettings

instance GenValid BakeSettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked AbsP

instance GenValid AbsP where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked ID

instance GenValid ID where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
