module Types where

import           Control.Monad.State.Lazy  (StateT)
import           Control.Monad.Writer.Lazy (Writer)


type Repo = String

type Host = String

data GitRepo = GitRepo {
        repo_protocol :: GitProtocol
    ,   repo_host     :: Host
    ,   repo_path     :: FilePath
    } deriving (Eq)

instance Show GitRepo where
    show repo = case repo_protocol repo of
        HTTPS -> "https://" ++ h ++ "/" ++ p
        Git -> "git@" ++ h ++ ":" ++ p ++ ".git"
      where
        p = repo_path repo
        h = repo_host repo

data GitProtocol = HTTPS | Git
    deriving (Show, Eq)

---[ Parsing Types ]---

type CardIdentifier = String
type CardName = Maybe CardIdentifier
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card CardName [Declaration]
    deriving (Show, Eq)

data DeploymentKind = LinkDeployment
                    | CopyDeployment
                    | UnspecifiedDeployment
    deriving (Show, Eq)

data SparkTarget = TargetGit GitRepo
                 | TargetCardName CardIdentifier
    deriving (Show, Eq)

data Declaration = SparkOff SparkTarget
                 | Deploy Source Destination DeploymentKind
                 | IntoDir Directory
                 | OutofDir Directory
                 | Block [Declaration]
    deriving (Show, Eq)

---[ Compiling Types ]---
data Deployment = Copy FilePath FilePath
                | Link FilePath FilePath
                | Spark SparkTarget
    deriving (Show, Eq)

type SparkCompiler = StateT CompilerState (Writer [Deployment])

data CompilerState = CompilerState {
        state_declarations_left        :: [Declaration]
    ,   state_deployment_kind_override :: DeploymentKind
    ,   state_into_prefix              :: FilePath
    ,   state_outof_prefix             :: FilePath
    } deriving (Show, Eq)
