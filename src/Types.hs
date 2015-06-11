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
import           Text.Parsec            (ParseError, ParsecT, getState,
                                         runParserT)


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
    deriving (Show, Eq)

runSparker :: SparkConfig -> Sparker a -> IO (Either SparkError a)
runSparker conf func = runReaderT (runExceptT func) conf


---[ Parsing Types ]---

type SparkParser = ParsecT String ParseState Sparker
data ParseState = ParseState {
        state_starting_file :: FilePath
    ,   state_current_file  :: FilePath
    }
runSparkParser :: ParseState -> SparkParser a -> String -> Sparker a
runSparkParser state func str = do
    e <- runParserT func state (state_current_file state) str
    case e of
        Left parseError -> throwError $ ParseError parseError
        Right a -> return a

getStates :: (ParseState -> a) -> SparkParser a
getStates f = do
    s <- getState
    return $ f s


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

data Declaration = SparkOff CardReference
                 | Deploy Source Destination DeploymentKind
                 | IntoDir Directory
                 | OutofDir Directory
                 | DeployKindOverride DeploymentKind
                 | Block [Declaration]
    deriving (Show, Eq)


---[ Compiling Types ]---
data Deployment = Copy FilePath FilePath
                | Link FilePath FilePath
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
    ,   state_deployment_kind_override :: DeploymentKind
    ,   state_into_prefix              :: FilePath
    ,   state_outof_prefix             :: FilePath
    } deriving (Show, Eq)


---[ Deploying Types ]---

type SparkDeployer = StateT DeployerState Sparker
data DeployerState = DeployerState {
        state_deployments_left :: [Deployment]
    }
data DeployError = PreDeployError String
                 | DuringDeployError String
                 | PostDeployError String
    deriving (Show, Eq)


runSparkDeployer :: DeployerState -> SparkDeployer a -> Sparker (a, DeployerState)
runSparkDeployer state func = runStateT func state

---[ Pretty Types ]---

type SparkFormatter = StateT FormatterState (WriterT String Sparker)
data FormatterState = FormatterState {
        state_current_indent        :: Int
    ,   state_longest_src           :: Int
    ,   state_newline_before_deploy :: Bool
    }

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
