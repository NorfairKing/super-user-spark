{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Parser.Types where

import Import

import qualified Text.Parsec as Parsec

newtype ParseAssignment = ParseAssignment
    { fileToParse :: Path Abs File
    } deriving (Show, Eq, Generic)

instance Validity ParseAssignment

data ParseSettings =
    ParseSettings
    deriving (Show, Eq, Generic)

instance Validity ParseSettings

newtype ParseError =
    ParseError Parsec.ParseError
    deriving (Show, Eq, Generic)

instance Validity ParseError where
    validate = trivialValidation
