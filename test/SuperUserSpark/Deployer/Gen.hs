{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Deployer.Gen where

import TestImport

import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Deployer.Types

instance GenUnchecked DeployAssignment

instance GenValid DeployAssignment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked DeploySettings

instance GenValid DeploySettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked PreDeployment

instance GenValid PreDeployment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
