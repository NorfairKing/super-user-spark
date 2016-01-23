module Deployer.Types where

import           Monad
import           System.Directory      (Permissions (..))
import           System.FilePath.Posix (takeExtension)

import           Parser.Types
import           Types


type CheckerCardReference = DeployerCardReference

data DeployerCardReference = DeployerCardCompiled CompiledCardReference
                           | DeployerCardUncompiled CardFileReference
    deriving (Show, Eq)

instance Read DeployerCardReference where
    readsPrec _ fp = case length (words fp) of
                      0 -> []
                      1 -> if takeExtension fp == ".sus"
                            then [(DeployerCardUncompiled (CardFileReference fp Nothing) ,"")]
                            else [(DeployerCardCompiled fp, "")]
                      2 -> let [f, c] = words fp
                            in [(DeployerCardUncompiled (CardFileReference f (Just $ CardNameReference c)), "")]
                      _ -> []

type SparkDeployer = StateT DeployerState Sparker
data DeployerState = DeployerState

runSparkDeployer :: DeployerState -> SparkDeployer a -> Sparker (a, DeployerState)
runSparkDeployer state func = runStateT func state

data Diagnostics = NonExistent
                 | IsFile Permissions
                 | IsDirectory Permissions
                 | IsLink Permissions
                 | IsPipe
                 | IsSocket
                 | IsCharDevice
                 | IsBlockDevice
    deriving (Show, Eq)

data PreDeployment = Ready FilePath FilePath DeploymentKind
                   | AlreadyDone
                   | Error String
    deriving (Show, Eq)

data ID = Plain String
        | Var String
    deriving (Show, Eq)


