{-# OPTIONS_GHC -Wno-orphans #-}

module Compiler.Gen where

import Compiler.Types
import Language.Gen ()
import Test.QuickCheck

instance Arbitrary Deployment where
    arbitrary = Put <$> arbitrary <*> arbitrary <*> arbitrary
