{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Compiler.Gen where

import TestImport

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Gen ()

instance Arbitrary Deployment where
    arbitrary = Put <$> arbitrary <*> arbitrary <*> arbitrary
