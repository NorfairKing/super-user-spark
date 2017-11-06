{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Diagnose.Gen where

import TestImport

import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Diagnose.Types
import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Language.Gen ()

instance GenUnchecked DiagnoseAssignment

instance GenValid DiagnoseAssignment where
    genValid = DiagnoseAssignment <$> genValid <*> genValid

instance GenUnchecked DiagnoseSettings

instance GenValid DiagnoseSettings where
    genValid = DiagnoseSettings <$> genValid

instance GenUnchecked DiagnoseError

instance GenValid DiagnoseError

instance GenUnchecked HashDigest

instance GenValid HashDigest

instance GenUnchecked Diagnostics

instance GenValid Diagnostics

instance GenUnchecked DiagnosedFp

instance GenValid DiagnosedFp where
    genValid = D <$> genValid <*> genValid <*> genValid
