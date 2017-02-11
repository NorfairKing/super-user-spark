module SuperUserSpark.Dispatch.Types where

import Import

import SuperUserSpark.Config.Types
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types

type Instructions = (Dispatch, SparkConfig)

data Dispatch
    = DispatchParse FilePath
    | DispatchCompile CardFileReference
    | DispatchCheck DeployerCardReference
    | DispatchDeploy DeployerCardReference
    deriving (Show, Eq)

type Options = (Dispatch, GlobalOptions)
