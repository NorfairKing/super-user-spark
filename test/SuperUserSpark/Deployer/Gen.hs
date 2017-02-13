{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Deployer.Gen where

import TestImport

import SuperUserSpark.Deployer.Types
import SuperUserSpark.Check.Gen ()

instance GenUnchecked DeployAssignment

instance GenValid DeployAssignment

instance Arbitrary DeployAssignment where
    arbitrary = genValid

instance GenUnchecked DeploySettings

instance GenValid DeploySettings

instance Arbitrary DeploySettings where
    arbitrary = genValid

instance GenUnchecked DeployerCardReference

instance GenValid DeployerCardReference

instance Arbitrary DeployerCardReference where
    arbitrary = genValid

instance GenUnchecked DeployError

instance GenValid DeployError

instance Arbitrary DeployError where
    arbitrary = genValid

instance GenUnchecked PreDeployment

instance GenValid PreDeployment

instance Arbitrary PreDeployment where
    arbitrary = genValid
