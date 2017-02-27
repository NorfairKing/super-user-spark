{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Deployer.Gen where

import TestImport

import SuperUserSpark.Check.Gen ()
import SuperUserSpark.Deployer.Types

instance GenUnchecked DeployAssignment

instance GenValid DeployAssignment where
    genValid = DeployAssignment <$> genValid <*> genValid

instance GenUnchecked DeploySettings

instance GenValid DeploySettings where
    genValid =
        DeploySettings <$> genValid <*> genValid <*> genValid <*> genValid


instance GenUnchecked DeployError

instance GenValid DeployError


