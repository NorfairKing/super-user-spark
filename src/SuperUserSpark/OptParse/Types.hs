{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.OptParse.Types where

import Import

data Dispatch
    = DispatchParse ParseArgs
    | DispatchCompile CompileArgs
    | DispatchBake BakeArgs
    | DispatchDiagnose DiagnoseArgs
    | DispatchCheck CheckArgs
    | DispatchDeploy DeployArgs
    deriving (Show, Eq, Generic)

instance Validity Dispatch

data ParseArgs = ParseArgs
    { parseFilePath :: FilePath
    } deriving (Show, Eq, Generic)

instance Validity ParseArgs

data CompileArgs = CompileArgs
    { compileArgCardRef :: String
    , compileArgOutput :: Maybe FilePath
    , compileFlags :: CompileFlags
    } deriving (Show, Eq, Generic)

instance Validity CompileArgs

data CompileFlags = CompileFlags
    { compileDefaultKind :: Maybe String
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

data DiagnoseArgs = DiagnoseArgs
    { diagnoseArgCardRef :: String
    , diagnoseFlags :: DiagnoseFlags
    } deriving (Show, Eq, Generic)

instance Validity DiagnoseArgs

data DiagnoseFlags = DiagnoseFlags
    { diagnoseBakeFlags :: BakeFlags
    } deriving (Show, Eq, Generic)

instance Validity DiagnoseFlags

data CheckArgs = CheckArgs
    { checkArgCardRef :: String
    , checkFlags :: CheckFlags
    } deriving (Show, Eq, Generic)

instance Validity CheckArgs

data CheckFlags = CheckFlags
    { checkDiagnoseFlags ::DiagnoseFlags
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
