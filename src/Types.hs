{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Applicative
import           Control.Monad          (mzero)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.State    (StateT, get, gets, modify, put,
                                         runStateT)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)

import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         object, (.:), (.=))

import           Data.Binary            (Binary (..), Get)
import qualified Data.Binary            as B
import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  (pack, unpack)
import           System.Directory       (Permissions (..))
import           Text.Parsec            (ParseError)

import           Constants

---[ Cards ]---
type CardName = String
type Source = FilePath
type Destination = FilePath
type Directory = FilePath

data Card = Card {
        card_name    :: CardName
    ,   card_path    :: FilePath
    ,   card_content :: Declaration
    } deriving (Show, Eq)

---[ Declarations ]---
data DeploymentKind = LinkDeployment
                    | CopyDeployment
    deriving (Show, Eq)

instance Binary DeploymentKind where
    put LinkDeployment = B.put True
    put CopyDeployment = B.put False
    get = do
        b <- B.get :: Get Bool
        return $ if b
        then LinkDeployment
        else CopyDeployment

instance Read DeploymentKind where
    readsPrec _ "link" = [(LinkDeployment,"")]
    readsPrec _ "copy" = [(CopyDeployment,"")]
    readsPrec _ _ = []

instance FromJSON DeploymentKind where
    parseJSON (String "link") = return LinkDeployment
    parseJSON (String "copy") = return CopyDeployment
    parseJSON _ = mzero

instance ToJSON DeploymentKind where
    toJSON LinkDeployment = String "link"
    toJSON CopyDeployment = String "copy"



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

-- To start, a card can't be referenced by its name.
type StartingSparkReference = CardFileReference

type CompilerCardReference = CardFileReference

type CompiledCardReference = FilePath

data CheckerCardReference = CheckerCardCompiled CompiledCardReference
                          | CheckerCardUncompiled CardFileReference
    deriving (Show, Eq)

data DeployerCardReference = DeployerCardCompiled CompiledCardReference
                           | DeployerCardUncompiled StartingSparkReference
    deriving (Show, Eq)

data CardReference = CardFile CardFileReference
                   | CardName CardNameReference
    deriving (Show, Eq)

---[ Base monad ]---

type Sparker = ExceptT SparkError (ReaderT SparkConfig IO)

---[ Options ]---

data Options = Options {
    opt_command :: Command
  , opt_global  :: GlobalOptions
  } deriving (Show, Eq)

data Command = CommandParse String
             | CommandFormat String
             | CommandCompile String
             | CommandCheck String
             | CommandDeploy String
    deriving (Show, Eq)

data GlobalOptions = GlobalOptions {
    opt_lineUp              :: Bool
  , opt_indent              :: Int
  , opt_trailingNewline     :: Bool
  , opt_alwaysQuote         :: Bool
  , opt_compress            :: Bool
  , opt_output              :: Maybe FilePath
  , opt_format              :: CompileFormat
  , opt_kind                :: Maybe DeploymentKind
  , opt_overrride           :: Maybe DeploymentKind
  , opt_thoroughness        :: CheckThoroughness
  , opt_replace_links       :: Bool
  , opt_replace_files       :: Bool
  , opt_replace_directories :: Bool
  , opt_replace             :: Bool
  , opt_debug               :: Bool
  } deriving (Show, Eq)

---[ Instructions ]--
type Instructions = (Dispatch, SparkConfig)

---[ Config ]---

data SparkConfig = Config {
        conf_format_lineUp              :: Bool
    ,   conf_format_indent              :: Int
    ,   conf_format_trailingNewline     :: Bool
    ,   conf_format_alwaysQuote         :: Bool
    ,   conf_format_oneLine             :: Bool
    ,   conf_compile_output             :: Maybe FilePath
    ,   conf_compile_format             :: CompileFormat
    ,   conf_compile_kind               :: Maybe DeploymentKind
    ,   conf_compile_override           :: Maybe DeploymentKind
    ,   conf_check_thoroughness         :: CheckThoroughness
    ,   conf_deploy_replace_links       :: Bool
    ,   conf_deploy_replace_files       :: Bool
    ,   conf_deploy_replace_directories :: Bool
    ,   conf_debug                      :: Bool
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

data CheckThoroughness = ThoroughnessName
                       | ThoroughnessChecksum
                       | ThoroughnessContent
    deriving (Show, Eq)

instance Read CheckThoroughness where
    readsPrec _ "name"       = [(ThoroughnessName,"")]
    readsPrec _ "checksum"   = [(ThoroughnessChecksum,"")]
    readsPrec _ "content"    = [(ThoroughnessContent,"")]
    readsPrec _ _ = []


data SparkError = ParseError ParseError
                | CompileError CompileError
                | DeployError DeployError
                | UnpredictedError String
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
    deriving Eq

instance Binary Deployment where
    put depl = do
        B.put $ map pack $ deployment_srcs depl
        B.put $ deployment_kind depl
        B.put $ pack $ deployment_dst depl
    get = do
        bsrcs <- B.get :: Get [ByteString]
        kind <- B.get :: Get DeploymentKind
        dst <- B.get :: Get ByteString
        return $ Put {
                deployment_srcs = map unpack bsrcs
            ,   deployment_kind = kind
            ,   deployment_dst = unpack dst
            }

instance Read Deployment where
    readsPrec _ str = [(Put {
                deployment_srcs = srcs
            ,   deployment_dst = dst
            ,   deployment_kind = kind
            }, "")]
      where
          srcs = (map unquote . reverse . drop 2 . reverse) ws
          kind = case lookup (last $ init ws) [(linkKindSymbol, LinkDeployment), (copyKindSymbol, CopyDeployment)] of
                    Nothing -> error "unrecognised deployment kind symbol"
                    Just k  -> k
          dst = last ws
          ws = words str
          unquote = tail . init

instance Show Deployment where
    show dep = unwords $ srcs ++ [k,dst]
      where
        srcs = map quote $ deployment_srcs dep
        k = case deployment_kind dep of
                LinkDeployment -> linkKindSymbol
                CopyDeployment -> copyKindSymbol
        dst = quote $ deployment_dst dep
        quote = (\s -> "\"" ++ s ++ "\"")

instance FromJSON Deployment where
    parseJSON (Object o) = Put <$> o .: "sources"
                               <*> o .: "destination"
                               <*> o .: "deployment kind"
    parseJSON _ = mzero

instance ToJSON Deployment where
    toJSON depl = object [
                           "sources" .= deployment_srcs depl
                         , "destination" .= deployment_dst depl
                         , "deployment kind" .= deployment_kind depl
                         ]



type CompileError = String

type SparkCompiler = StateT CompilerState (WriterT [Deployment] Sparker)

type CompilerPrefix = [PrefixPart]

data PrefixPart = Literal String
                | Alts [String]
    deriving (Show, Eq)

runSparkCompiler :: CompilerState -> SparkCompiler a -> Sparker ((a,CompilerState), [Deployment])
runSparkCompiler s func = runWriterT (runStateT func s)


data CompilerState = CompilerState {
        state_current_card             :: Card
    ,   state_current_directory        :: FilePath
    ,   state_all_cards                :: [Card]
    ,   state_declarations_left        :: [Declaration]
    ,   state_deployment_kind_override :: Maybe DeploymentKind
    ,   state_into                     :: Directory
    ,   state_outof_prefix             :: CompilerPrefix
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


