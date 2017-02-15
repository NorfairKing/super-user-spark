{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Deployer.Types where

import Import

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Types
import SuperUserSpark.CoreTypes

data DeployAssignment = DeployAssignment
    { deployCardReference :: BakeCardReference
    , deploySettings :: DeploySettings
    } deriving (Show, Eq, Generic)

instance Validity DeployAssignment

data DeploySettings = DeploySettings
    { deploySetsReplaceLinks :: Bool
    , deploySetsReplaceFiles :: Bool
    , deploySetsReplaceDirectories :: Bool
    , deployCheckSettings :: CheckSettings
    } deriving (Show, Eq, Generic)

instance Validity DeploySettings

defaultDeploySettings :: DeploySettings
defaultDeploySettings =
    DeploySettings
    { deploySetsReplaceLinks = False
    , deploySetsReplaceFiles = False
    , deploySetsReplaceDirectories = False
    , deployCheckSettings = defaultCheckSettings
    }

type SparkDeployer = ExceptT DeployError (ReaderT DeploySettings IO)

data DeployError
    = DeployCheckError CheckError
    | DeployError String
    deriving (Show, Eq, Generic)

instance Validity DeployError

data PreDeployment
    = Ready FilePath
            FilePath
            DeploymentKind
    | AlreadyDone
    | Error String
    deriving (Show, Eq, Generic)

instance Validity PreDeployment
