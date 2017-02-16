{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Compiler.Gen where

import TestImport

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Gen ()
import SuperUserSpark.PreCompiler.Gen ()

instance GenUnchecked CompileAssignment

instance GenValid CompileAssignment where
    genValid = CompileAssignment <$> genValid <*> genValid <*> genValid


instance GenUnchecked StrongCardFileReference

instance GenValid StrongCardFileReference


instance GenUnchecked CompileSettings

instance GenValid CompileSettings where
    genValid = CompileSettings <$> genValid <*> genValid


instance GenUnchecked a =>
         GenUnchecked (Deployment a) where
    genUnchecked = Deployment <$> genUnchecked <*> genUnchecked

instance GenValid a =>
         GenValid (Deployment a) where
    genValid = Deployment <$> genValid <*> genValid

instance GenUnchecked a =>
         GenUnchecked (DeploymentDirections a)

instance GenValid a =>
         GenValid (DeploymentDirections a) where
    genValid = Directions <$> genValid <*> genValid

instance GenUnchecked PrefixPart

instance GenValid PrefixPart


instance GenUnchecked CompilerState

instance GenValid CompilerState

instance GenUnchecked CompileError where
    genUnchecked =
        oneof
            [ PreCompileErrors <$> genUnchecked
            , DuringCompilationError <$> genUnchecked
            ]

instance GenValid CompileError

