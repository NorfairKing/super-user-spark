{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Bake.Gen where

import TestImport

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Gen ()

instance GenUnchecked BakeAssignment

instance GenValid BakeAssignment where
    genValid = BakeAssignment <$> genValid <*> genValid

instance GenUnchecked BakeCardReference

instance GenValid BakeCardReference

instance GenUnchecked BakeSettings

instance GenValid BakeSettings

instance GenUnchecked BakeError

instance GenValid BakeError

instance GenUnchecked BakedDeployment

instance GenValid BakedDeployment where
    genValid = BakedDeployment <$> genValid <*> genValid

instance GenUnchecked AbsP

instance GenValid AbsP

instance Arbitrary AbsP where
    arbitrary = genValid

instance GenUnchecked a =>
         GenUnchecked (DeploymentDirections a)

instance GenValid a =>
         GenValid (DeploymentDirections a) where
    genValid = Directions <$> genValid <*> genValid

instance GenUnchecked ID

instance GenValid ID
