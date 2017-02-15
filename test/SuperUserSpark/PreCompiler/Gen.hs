{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.PreCompiler.Gen where

import TestImport

import SuperUserSpark.Language.Gen ()
import SuperUserSpark.PreCompiler.Types

instance GenUnchecked PreCompileError

instance GenValid PreCompileError
