{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Diagnose.Types where

import Import

import Data.Aeson
import Data.Hashable
import Text.Printf

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Types

data DiagnoseAssignment = DiagnoseAssignment
    { diagnoseCardReference :: BakeCardReference
    , diagnoseSettings :: DiagnoseSettings
    } deriving (Show, Eq, Generic)

instance Validity DiagnoseAssignment

data DiagnoseSettings = DiagnoseSettings
    { diagnoseBakeSettings :: BakeSettings
    } deriving (Show, Eq, Generic)

instance Validity DiagnoseSettings

defaultDiagnoseSettings :: DiagnoseSettings
defaultDiagnoseSettings =
    DiagnoseSettings {diagnoseBakeSettings = defaultBakeSettings}

type SparkDiagnoser = ExceptT DiagnoseError (ReaderT DiagnoseSettings IO)

data DiagnoseError
    = DiagnoseBakeError BakeError
    | DiagnoseError String
    deriving (Show, Eq, Generic)

instance Validity DiagnoseError

newtype HashDigest =
    HashDigest Int
    deriving (Show, Eq, Generic)

instance Validity HashDigest

instance Monoid HashDigest where
    mempty = HashDigest (hash ())
    (HashDigest h1) `mappend` (HashDigest h2) = HashDigest $ h1 * 31 + h2

instance Hashable HashDigest

instance ToJSON HashDigest where
    toJSON (HashDigest i) = toJSON $ (printf "%016x" i :: String)

data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLinkTo AbsP -- Could point to directory too.
    | IsWeird
    deriving (Show, Eq, Generic)

instance Validity Diagnostics

instance ToJSON Diagnostics where
    toJSON Nonexistent = String "nonexistent"
    toJSON IsFile = String "file"
    toJSON IsDirectory = String "directory"
    toJSON (IsLinkTo ap) =
        object ["kind" .= String "link", "link destination" .= ap]
    toJSON IsWeird = String "weird"

data DiagnosedFp = D
    { diagnosedFilePath :: AbsP
    , diagnosedDiagnostics :: Diagnostics
    , diagnosedHashDigest :: HashDigest
    } deriving (Show, Eq, Generic)

instance Validity DiagnosedFp

instance ToJSON DiagnosedFp where
    toJSON D {..} =
        object $
        ["path" .= diagnosedFilePath, "diagnostics" .= diagnosedDiagnostics] ++
        if diagnosedHashDigest == mempty
            then []
            else ["hash" .= diagnosedHashDigest]

type DiagnosedDeployment = Deployment DiagnosedFp
