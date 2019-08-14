{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Compiler.Gen where

import TestImport

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Gen ()
import SuperUserSpark.Parser.Gen ()
import SuperUserSpark.PreCompiler.Gen ()

instance GenUnchecked CompileAssignment

instance GenValid CompileAssignment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked StrongCardFileReference

instance GenValid StrongCardFileReference where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked CompileSettings

instance GenValid CompileSettings where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (Deployment a) where
  genUnchecked = Deployment <$> genUnchecked <*> genUnchecked

instance GenValid a => GenValid (Deployment a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (DeploymentDirections a)

instance GenValid a => GenValid (DeploymentDirections a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked PrefixPart

instance GenValid PrefixPart where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked CompilerState

instance GenValid CompilerState where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
