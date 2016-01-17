{-# LANGUAGE OverloadedStrings #-}
module Types
    (
      module Types
    , module CoreTypes
    , module Monad
    , module Config.Types

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

import           Config.Types
import           CoreTypes
import           Monad
import           Parser.Types

---[ Card References ]--
type CompilerCardReference = CardFileReference

type CompiledCardReference = FilePath

instance Read CardFileReference where
    readsPrec _ fp = case length (words fp) of
                      1 -> [(CardFileReference fp Nothing ,"")]
                      2 -> let [f, c] = words fp
                            in [(CardFileReference f (Just $ CardNameReference c), "")]
                      _ -> []

---[ Options ]---

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


