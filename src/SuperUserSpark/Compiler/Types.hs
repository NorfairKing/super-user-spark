{-# LANGUAGE OverloadedStrings #-}

module SuperUserSpark.Compiler.Types where

import Import

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import SuperUserSpark.Config.Types
import SuperUserSpark.Constants
import Control.Monad (mzero)
import SuperUserSpark.CoreTypes
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, (.:), (.=))
import SuperUserSpark.Language.Types
import SuperUserSpark.Monad

data Deployment = Put
    { deploymentSources :: [FilePath]
    , deploymentDestination :: FilePath
    , deploymentKind :: DeploymentKind
    } deriving (Eq)

instance Show Deployment where
    show dep = unwords $ srcs ++ [k, dst]
      where
        srcs = map quote $ deploymentSources dep
        k =
            case deploymentKind dep of
                LinkDeployment -> linkKindSymbol
                CopyDeployment -> copyKindSymbol
        dst = quote $ deploymentDestination dep
        quote = (\s -> "\"" ++ s ++ "\"")

instance FromJSON Deployment where
    parseJSON (Object o) =
        Put <$> o .: "sources" <*> o .: "destination" <*> o .: "deployment kind"
    parseJSON _ = mzero

instance ToJSON Deployment where
    toJSON depl =
        object
            [ "sources" .= deploymentSources depl
            , "destination" .= deploymentDestination depl
            , "deployment kind" .= deploymentKind depl
            ]

type CompilerPrefix = [PrefixPart]

data PrefixPart
    = Literal String
    | Alts [String]
    deriving (Show, Eq)

data CompilerState = CompilerState
    { stateDeploymentKindOverride :: Maybe DeploymentKind
    , stateInto :: Directory
    , stateOutof_prefix :: CompilerPrefix
    } deriving (Show, Eq)

type PrecompileError = String

type ImpureCompiler = ExceptT CompileError (ReaderT SparkConfig IO)

type PureCompiler = ExceptT CompileError (ReaderT SparkConfig Identity)

type Precompiler = WriterT [PrecompileError] Identity

type InternalCompiler = StateT CompilerState (WriterT ([Deployment], [CardReference]) PureCompiler)
