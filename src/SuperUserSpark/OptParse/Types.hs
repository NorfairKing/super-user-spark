module SuperUserSpark.OptParse.Types where

import Import

import SuperUserSpark.CoreTypes

data GlobalOptions = GlobalOptions
    { optOutput :: Maybe FilePath
    , optKind :: Maybe DeploymentKind
    , optOverride :: Maybe DeploymentKind
    , optReplaceLinks :: Bool
    , optReplaceFiles :: Bool
    , optReplaceDirectories :: Bool
    , optReplace :: Bool
    , optDebug :: Bool
    } deriving (Show, Eq)
