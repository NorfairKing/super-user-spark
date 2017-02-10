module Dispatch.Types where

import Import

import Types

import Config.Types
import Deployer.Types
import Language.Types

type Instructions = (Dispatch, SparkConfig)

data Dispatch
    = DispatchParse FilePath
    | DispatchCompile CardFileReference
    | DispatchCheck DeployerCardReference
    | DispatchDeploy DeployerCardReference
    deriving (Show, Eq)

type Options = (Dispatch, GlobalOptions)
