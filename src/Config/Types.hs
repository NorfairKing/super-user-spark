module Config.Types where

import Import

import CoreTypes

data SparkConfig = Config
    { confCompileOutput :: Maybe FilePath
    , confCompileKind :: Maybe DeploymentKind
    , confCompileOverride :: Maybe DeploymentKind
    , confDeployReplaceLinks :: Bool
    , confDeployReplaceFiles :: Bool
    , confDeployReplaceDirectories :: Bool
    , confDebug :: Bool
    } deriving (Show, Eq)
