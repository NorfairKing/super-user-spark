{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.OptParse.Types where

import Import

data Dispatch
    = DispatchParse ParseArgs
    | DispatchCompile CompileArgs
    | DispatchCheck CheckArgs
    | DispatchDeploy DeployArgs
    deriving (Show, Eq)

data ParseArgs = ParseArgs
    { parseFilePath :: FilePath
    } deriving (Show, Eq, Generic)

data CompileArgs = CompileArgs
    { compileCardRef :: String
    , compileFlags :: CompileFlags
    } deriving (Show, Eq, Generic)

data CompileFlags = CompileFlags
    { compileFlagOutput :: Maybe FilePath
    , compileDefaultKind :: Maybe String
    , compileKindOverride :: Maybe String
    } deriving (Show, Eq, Generic)

data CheckArgs = CheckArgs
    { checkArgCardRef :: String
    , checkFlags :: CheckFlags
    } deriving (Show, Eq, Generic)

data CheckFlags = CheckFlags
    { checkCompileFlags :: CompileFlags
    } deriving (Show, Eq, Generic)

data DeployArgs = DeployArgs
    { deployArgCardRef :: String
    , deployFlags :: DeployFlags
    } deriving (Show, Eq, Generic)

data DeployFlags = DeployFlags
    { deployFlagReplaceLinks :: Bool
    , deployFlagReplaceFiles :: Bool
    , deployFlagReplaceDirectories :: Bool
    , deployFlagReplaceAll :: Bool
    , deployCheckFlags :: CheckFlags
    } deriving (Show, Eq, Generic)
