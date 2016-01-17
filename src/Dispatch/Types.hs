module Dispatch.Types where

import           Monad
import           Types

import           Deployer.Types


type Instructions = (Dispatch, SparkConfig)

data Dispatch = DispatchParse FilePath
              | DispatchFormat FilePath
              | DispatchCompile CompilerCardReference
              | DispatchCheck CheckerCardReference
              | DispatchDeploy DeployerCardReference
    deriving (Show, Eq)

type Options = (Dispatch, GlobalOptions)

