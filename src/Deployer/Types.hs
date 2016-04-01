module Deployer.Types where

import           Language.Types
import           Monad
import           System.FilePath.Posix (takeExtension)
import           Types

data DeployerCardReference
    = DeployerCardCompiled FilePath
    | DeployerCardUncompiled CardFileReference
    deriving (Show, Eq)

instance Read DeployerCardReference where
    readsPrec _ fp
        = case length (words fp) of
            0 -> []
            1 -> if takeExtension fp == ".sus"
                  then [(DeployerCardUncompiled (CardFileReference fp Nothing) ,"")]
                  else [(DeployerCardCompiled fp, "")]
            2 -> let [f, c] = words fp
                  in [(DeployerCardUncompiled (CardFileReference f (Just $ CardNameReference c)), "")]
            _ -> []

type SparkDeployer = StateT DeployerState Sparker
data DeployerState = DeployerState

data PreDeployment
    = Ready FilePath FilePath DeploymentKind
    | AlreadyDone
    | Error String
    deriving (Show, Eq)

data ID
    = Plain String
    | Var String
    deriving (Show, Eq)


