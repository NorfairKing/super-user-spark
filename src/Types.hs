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
import           Text.Parsec            (ParseError)

---[ Repositories ]---

type Host = String
type Branch = String

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



---[ Cards ]---
type CardName = String
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card {
        card_name    :: CardName
    ,   card_path    :: FilePath
    ,   card_content :: [Declaration]
    } deriving (Show, Eq)

---[ Declarations ]---
data DeploymentKind = LinkDeployment
                    | CopyDeployment
    deriving (Show, Eq)

instance Read DeploymentKind where
    readsPrec _ "link" = [(LinkDeployment,"")]
    readsPrec _ "copy" = [(CopyDeployment,"")]
    readsPrec _ _ = []

data Declaration = SparkOff CardReference
                 | Deploy Source Destination (Maybe DeploymentKind)
                 | IntoDir Directory
                 | OutofDir Directory
                 | DeployKindOverride DeploymentKind
                 | Alternatives [Directory]
                 | Block [Declaration]
    deriving (Show, Eq)

---[ Card References ]--

-- Reference a card by name.
data CardNameReference = CardNameReference CardName
    deriving (Show, Eq)

-- Reference a card by the file it's in and therein potentially by a name reference
data CardFileReference = CardFileReference FilePath (Maybe CardNameReference)
    deriving (Show, Eq)

-- Reference a card by the git repository it's in, therein potentially by a file reference
data CardRepoReference = CardRepoReference GitRepo (Maybe Branch) (Maybe CardFileReference)
    deriving (Show, Eq)

-- To start, a card can't be referenced by its name.
data StartingSparkReference = StartFile CardFileReference
                            | StartRepo CardRepoReference
    deriving (Show, Eq)

type CompilerCardReference = CardFileReference

type CompiledCardReference = FilePath

data CheckerCardReference = CheckerCardCompiled CompiledCardReference
                          | CheckerCardUncompiled CardFileReference
    deriving (Show, Eq)

data DeployerCardReference = DeployerCardCompiled CompiledCardReference
                           | DeployerCardUncompiled StartingSparkReference
    deriving (Show, Eq)

data CardReference = CardRepo CardRepoReference
                   | CardFile CardFileReference
                   | CardName CardNameReference
    deriving (Show, Eq)

---[ Base monad ]---

type Sparker = ExceptT SparkError (ReaderT SparkConfig IO)
data SparkConfig = Config {
        conf_format  :: FormatOptions
    ,   conf_compile :: CompileOptions
    ,   conf_check   :: CheckOptions
    ,   conf_deploy  :: DeployOptions
    } deriving (Show, Eq)

data FormatOptions = FormatOptions {
        conf_format_lineUp          :: Bool
    ,   conf_format_indent          :: Int
    ,   conf_format_trailingNewline :: Bool
    ,   conf_format_alwaysQuote     :: Bool
    } deriving (Show, Eq)

data CompileOptions = CompileOptions {
        conf_compile_output :: Maybe FilePath
    ,   conf_compile_format :: CompileFormat
    } deriving (Show, Eq)

data CompileFormat = FormatBinary
                   | FormatText
                   | FormatJson
                   | FormatStandalone
    deriving (Show, Eq)

instance Read CompileFormat where
    readsPrec _ "binary"     = [(FormatBinary,"")]
    readsPrec _ "text"       = [(FormatText,"")]
    readsPrec _ "json"       = [(FormatJson,"")]
    readsPrec _ "standalone" = [(FormatStandalone,"")]
    readsPrec _ _ = []

data CheckOptions = CheckOptions {
        conf_check_thoroughness :: CheckThoroughness
    } deriving (Show, Eq)

data CheckThoroughness = ThoroughnessName
                       | ThoroughnessChecksum
                       | ThoroughnessContent
    deriving (Show, Eq)

instance Read CheckThoroughness where
    readsPrec _ "name"       = [(ThoroughnessName,"")]
    readsPrec _ "checksum"   = [(ThoroughnessChecksum,"")]
    readsPrec _ "content"    = [(ThoroughnessContent,"")]
    readsPrec _ _ = []

data DeployOptions = DeployOptions {
        conf_deploy_kind         :: DeploymentKind
    ,   conf_deploy_override     :: Maybe DeploymentKind
    ,   conf_replace_links       :: Bool
    ,   conf_replace_files       :: Bool
    ,   conf_replace_directories :: Bool
    } deriving (Show, Eq)


data SparkError = ParseError ParseError
                | CompileError CompileError
                | DeployError DeployError
                | UnpredictedError String
                | GitError GitError
    deriving Show

runSparker :: SparkConfig -> Sparker a -> IO (Either SparkError a)
runSparker conf func = runReaderT (runExceptT func) conf

---[ Dispatching ]---

data Dispatch = DispatchParse FilePath
              | DispatchFormat FilePath
              | DispatchCompile CompilerCardReference
              | DispatchCheck CheckerCardReference
              | DispatchDeploy DeployerCardReference
    deriving (Show, Eq)


---[ Compiling Types ]---
data Deployment = Put {
        deployment_srcs :: [FilePath]
    ,   deployment_dst  :: FilePath
    ,   deployment_kind :: DeploymentKind
    }
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
data DeployerState = DeployerState
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
    deriving (Show, Eq)

data PreDeployment = Ready FilePath FilePath DeploymentKind
                   | AlreadyDone
                   | Error String
    deriving (Show, Eq)

data ID = Plain String
        | Var String
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


