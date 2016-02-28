module Dispatch.Types where

import           Types

import           Deployer.Types
import           Language.Types


type Instructions = (Dispatch, SparkConfig)

data Dispatch = DispatchParse FilePath
              | DispatchFormat FilePath
              | DispatchCompile CardFileReference
              | DispatchCheck DeployerCardReference
              | DispatchDeploy DeployerCardReference
    deriving (Show, Eq)

type Options = (Dispatch, GlobalOptions)

