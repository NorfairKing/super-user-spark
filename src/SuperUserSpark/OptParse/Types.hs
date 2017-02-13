{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.OptParse.Types where

import Import

data Dispatch
    = DispatchParse ParseArgs
    | DispatchCompile CompileArgs
    | DispatchBake BakeArgs
    | DispatchCheck CheckArgs
    | DispatchDeploy DeployArgs
    deriving (Show, Eq, Generic)

instance Validity Dispatch

data ParseArgs = ParseArgs
    { parseFilePath :: FilePath
    } deriving (Show, Eq, Generic)

instance Validity ParseArgs

data CompileArgs = CompileArgs
    { compileCardRef :: String
    , compileFlags :: CompileFlags
    } deriving (Show, Eq, Generic)

instance Validity CompileArgs

data CompileFlags = CompileFlags
    { compileFlagOutput :: Maybe FilePath
    , compileDefaultKind :: Maybe String
    , compileKindOverride :: Maybe String
    } deriving (Show, Eq, Generic)

instance Validity CompileFlags

data BakeArgs = BakeArgs
    { bakeCardRef :: String
    , bakeFlags :: BakeFlags
    } deriving (Show, Eq, Generic)

instance Validity BakeArgs

data BakeFlags = BakeFlags
    { bakeCompileFlags :: CompileFlags
    } deriving (Show, Eq, Generic)

instance Validity BakeFlags

data CheckArgs = CheckArgs
    { checkArgCardRef :: String
    , checkFlags :: CheckFlags
    } deriving (Show, Eq, Generic)

instance Validity CheckArgs

data CheckFlags = CheckFlags
    { checkBakeFlags :: BakeFlags
    } deriving (Show, Eq, Generic)

instance Validity CheckFlags

data DeployArgs = DeployArgs
    { deployArgCardRef :: String
    , deployFlags :: DeployFlags
    } deriving (Show, Eq, Generic)

instance Validity DeployArgs

data DeployFlags = DeployFlags
    { deployFlagReplaceLinks :: Bool
    , deployFlagReplaceFiles :: Bool
    , deployFlagReplaceDirectories :: Bool
    , deployFlagReplaceAll :: Bool
    , deployCheckFlags :: CheckFlags
    } deriving (Show, Eq, Generic)

instance Validity DeployFlags
