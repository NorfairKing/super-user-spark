module Types
    (
      module Types
    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Writer
    , module Control.Monad.Trans
    , module Text.Parsec
    ) where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.State    (StateT, get, gets, modify, put,
                                         runStateT)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)

import           System.Directory       (Permissions (..))
import           System.Posix.Files     (FileStatus)
import           Text.Parsec            (ParseError)


data StartingSparkReference = FileRef FilePath (Maybe CardName)
                            | RepoRef GitRepo (Maybe Branch) (Maybe (FilePath, Maybe CardName))
    deriving (Show, Eq)

type Branch = String
data CardReference = CardRepo GitRepo (Maybe Branch) (Maybe (FilePath, Maybe CardName))
                   | CardFile FilePath (Maybe CardName)
                   | CardName CardName
    deriving (Show, Eq)


---[ Base monad ]---

type Sparker = ExceptT SparkError (ReaderT SparkConfig IO)
data SparkConfig = Config {
        conf_dry :: Bool
    } deriving (Show, Eq)

data SparkError = ParseError ParseError
                | CompileError CompileError
                | DeployError DeployError
                | UnpredictedError String
                | GitError GitError
    deriving (Show, Eq)

runSparker :: SparkConfig -> Sparker a -> IO (Either SparkError a)
runSparker conf func = runReaderT (runExceptT func) conf



---[ Parsing Types ]---

type CardName = String
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card CardName FilePath [Declaration]
    deriving (Show, Eq)

card_name :: Card -> CardName
card_name (Card n _ _) = n

data DeploymentKind = LinkDeployment
                    | CopyDeployment
    deriving (Show, Eq)

data Declaration = SparkOff CardReference
                 | Deploy Source Destination (Maybe DeploymentKind)
                 | IntoDir Directory
                 | OutofDir Directory
                 | DeployKindOverride DeploymentKind
                 | Alternatives [Directory]
                 | Block [Declaration]
    deriving (Show, Eq)


---[ Compiling Types ]---
data Deployment = Put [FilePath] FilePath DeploymentKind
    deriving (Show, Eq)

type CompileError = String

type SparkCompiler = StateT CompilerState (WriterT [Deployment] Sparker)

runSparkCompiler :: CompilerState -> SparkCompiler a -> Sparker ((a,CompilerState), [Deployment])
runSparkCompiler s func = runWriterT (runStateT func s)


data CompilerState = CompilerState {
        state_current_card             :: Card
    ,   state_current_directory        :: FilePath
    ,   state_all_cards                :: [Card]
    ,   state_declarations_left        :: [Declaration]
    ,   state_deployment_kind_override :: Maybe DeploymentKind
    ,   state_into_prefix              :: FilePath
    ,   state_outof_prefix             :: FilePath
    ,   state_alternatives             :: [Directory]
    } deriving (Show, Eq)


---[ Deploying Types ]---

type SparkDeployer = StateT DeployerState Sparker
data DeployerState = DeployerState {
        state_deployments    :: [Deployment]
    ,   state_predeployments :: [PreDeployment]
    }
data DeployError = PreDeployError [String]
                 | DuringDeployError [String]
                 | PostDeployError [String]
    deriving (Show, Eq)


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

data PreDeployment = Ready FilePath FilePath DeploymentKind
                   | AlreadyDone
                   | Warning String
                   | Error String
    deriving (Show, Eq)

---[ Pretty Types ]---

type SparkFormatter = StateT FormatterState (WriterT String Sparker)
data FormatterState = FormatterState {
        state_current_indent        :: Int
    ,   state_longest_src           :: Int
    ,   state_newline_before_deploy :: Bool
    }
    deriving (Show, Eq)

runSparkFormatter :: FormatterState -> SparkFormatter a -> Sparker ((a, FormatterState), String)
runSparkFormatter state func = runWriterT (runStateT func state)

---[ Repositories ]---

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

data GitError = GitRepoError GitRepo String
    deriving (Show, Eq)

