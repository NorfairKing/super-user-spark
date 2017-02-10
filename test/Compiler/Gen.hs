{-# OPTIONS_GHC -Wno-orphans #-}

module Compiler.Gen where

import TestImport

import Compiler.Types
import Language.Gen ()

instance Arbitrary Deployment where
    arbitrary = Put <$> arbitrary <*> arbitrary <*> arbitrary
