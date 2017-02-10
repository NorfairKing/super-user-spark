module Config where

import Import

import Config.Types

defaultConfig :: SparkConfig
defaultConfig =
    Config
    { confCompileOutput = Nothing
    , confCompileKind = Nothing
    , confCompileOverride = Nothing
    , confDeployReplaceLinks = False
    , confDeployReplaceFiles = False
    , confDeployReplaceDirectories = False
    , confDebug = False
    }
