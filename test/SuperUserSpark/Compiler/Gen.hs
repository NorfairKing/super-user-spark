{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Compiler.Gen where

import TestImport

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Gen ()

instance GenUnchecked CompileSettings where
    genUnchecked = CompileSettings <$> genUnchecked <*> genUnchecked <*> genUnchecked

instance GenValid CompileSettings

instance GenUnchecked Deployment where
    genUnchecked = Put <$> genUnchecked <*> genUnchecked <*> genUnchecked

instance GenValid Deployment

instance Arbitrary Deployment where
    arbitrary = genValid
