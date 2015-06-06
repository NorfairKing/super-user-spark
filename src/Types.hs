module Types where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (StateT)
import           Control.Monad.Writer (WriterT)
import           Text.Parsec          (Parsec)


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

type SparkParser = Parsec String ParseState

data ParseState = ParseState {
        state_current_file :: FilePath
    }

type CardName = String
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card CardName FilePath [Declaration]
    deriving (Show, Eq)

data DeploymentKind = LinkDeployment
                    | CopyDeployment
                    | UnspecifiedDeployment
    deriving (Show, Eq)

data SparkTarget = TargetGit GitRepo
                 | TargetCardName CardName
    deriving (Show, Eq)

data Declaration = SparkOff SparkTarget
                 | Deploy Source Destination DeploymentKind
                 | IntoDir Directory
                 | OutofDir Directory
                 | DeployKindOverride DeploymentKind
                 | Block [Declaration]
    deriving (Show, Eq)

---[ Compiling Types ]---
data Deployment = Copy FilePath FilePath
                | Link FilePath FilePath
                | Spark SparkTarget
    deriving (Show, Eq)

type SparkCompiler = StateT CompilerState (WriterT [Deployment] IO)

data CompilerState = CompilerState {
        state_current_card             :: Card
    ,   state_current_directory        :: FilePath
    ,   state_home_dir                 :: FilePath
    ,   state_all_cards                :: [Card]
    ,   state_declarations_left        :: [Declaration]
    ,   state_deployment_kind_override :: DeploymentKind
    ,   state_into_prefix              :: FilePath
    ,   state_outof_prefix             :: FilePath
    } deriving (Show, Eq)

---[ Deploying Types ]---

type SparkDeployer = ReaderT DeployConfig IO

data DeployConfig = DeployConfig
