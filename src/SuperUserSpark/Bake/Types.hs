{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Bake.Types where

import Import

import Data.Aeson
import System.FilePath (takeExtension)

import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types

data BakeAssignment = BakeAssignment
    { bakeCardReference :: BakeCardReference
    , bakeSettings :: BakeSettings
    } deriving (Show, Eq, Generic)

instance Validity BakeAssignment

data BakeCardReference
    = BakeCardCompiled (Path Abs File)
    | BakeCardUncompiled StrongCardFileReference
    deriving (Show, Eq, Generic)

instance Validity BakeCardReference

data BakeSettings = BakeSettings
    { bakeEnvironment :: [(String, String)]
    , bakeCompileSettings :: CompileSettings
    } deriving (Show, Eq, Generic)

instance Validity BakeSettings

defaultBakeSettings :: BakeSettings
defaultBakeSettings =
    BakeSettings
    {bakeEnvironment = [], bakeCompileSettings = defaultCompileSettings}

type SparkBaker = ExceptT BakeError (ReaderT BakeSettings IO)

data BakeError
    = BakeCompileError CompileError
    | BakeError String
    deriving (Show, Eq, Generic)

instance Validity BakeError

data BakedDeployment = BakedDeployment
    { bakedDirections :: DeploymentDirections AbsP AbsP
    , bakedKind :: DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity BakedDeployment

instance ToJSON BakedDeployment

instance FromJSON BakedDeployment

-- | An absolute path.
--
-- This is kept as a 'Path Abs File' to avoid existential quantification, but
-- that is an implementation detail and should not be exposed as functionality.
newtype AbsP = AbsP
    { unAbsP :: Path Abs File
    } deriving (Show, Eq, Generic)

instance Validity AbsP

instance ToJSON AbsP where
    toJSON (AbsP p) = toJSON p

instance FromJSON AbsP where
    parseJSON v = AbsP <$> parseJSON v

toPath :: AbsP -> FilePath
toPath = toFilePath . unAbsP

data DeploymentDirections a b = Directions
    { directionSources :: [a]
    , directionDestination :: b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) =>
         Validity (DeploymentDirections a b)

instance (ToJSON a, ToJSON b) =>
         ToJSON (DeploymentDirections a b)

instance (FromJSON a, FromJSON b) =>
         FromJSON (DeploymentDirections a b)

data ID
    = Plain String
    | Var String
    deriving (Show, Eq, Generic)

instance Validity ID
