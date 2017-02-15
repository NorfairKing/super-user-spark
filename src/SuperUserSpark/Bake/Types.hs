{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Bake.Types where

import Import

import Data.Aeson

import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes

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
    { bakeRoot :: Path Abs Dir
    , bakeEnvironment :: [(String, String)]
    , bakeCompileSettings :: CompileSettings
    } deriving (Show, Eq, Generic)

instance Validity BakeSettings

defaultBakeSettings :: BakeSettings
defaultBakeSettings =
    BakeSettings
    { bakeRoot = $(mkAbsDir "/")
    , bakeEnvironment = []
    , bakeCompileSettings = defaultCompileSettings
    }

type SparkBaker = ExceptT BakeError (ReaderT BakeSettings IO)

data BakeError
    = BakeCompileError CompileError
    | BakeError String
    deriving (Show, Eq, Generic)

instance Validity BakeError

data BakedDeployment = BakedDeployment
    { bakedDirections :: DeploymentDirections AbsP
    , bakedKind :: DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity BakedDeployment

instance ToJSON BakedDeployment where
    toJSON BakedDeployment {..} =
        object ["directions" .= bakedDirections, "deployment kind" .= bakedKind]

instance FromJSON BakedDeployment where
    parseJSON =
        withObject "Baked Deployment" $ \o ->
            BakedDeployment <$> o .: "directions" <*> o .: "deployment kind"

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

data ID
    = Plain String
    | Var String
    deriving (Show, Eq, Generic)

instance Validity ID
