{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Deployer.Gen where

import TestImport

import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Deployer.Types

instance GenUnchecked DeployAssignment

instance GenValid DeployAssignment where
    genValid = DeployAssignment <$> genValid <*> genValid

instance Arbitrary DeployAssignment where
    arbitrary = genValid

instance GenUnchecked DeploySettings

instance GenValid DeploySettings where
    genValid =
        DeploySettings <$> genValid <*> genValid <*> genValid <*> genValid

instance Arbitrary DeploySettings where
    arbitrary = genValid

instance GenUnchecked DeployError

instance GenValid DeployError

instance Arbitrary DeployError where
    arbitrary = genValid

instance GenUnchecked PreDeployment

instance GenValid PreDeployment

instance Arbitrary PreDeployment where
    arbitrary = genValid
