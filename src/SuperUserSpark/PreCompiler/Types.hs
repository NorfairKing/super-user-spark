{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.PreCompiler.Types where

import Import

newtype PreCompileError =
    PreCompileError String
    deriving (Show, Eq, Generic)

type Precompiler = WriterT [PreCompileError] Identity
