{-# LANGUAGE OverloadedStrings #-}
module Compiler.Types where

import           Constants
import           Control.Monad  (mzero)
import           CoreTypes
import           Data.Aeson     (FromJSON (..), ToJSON (..), Value (..), object,
                                 (.:), (.=))
import           Language.Types
import           Monad
import           Types

data Deployment = Put
    { deployment_srcs :: [FilePath]
    , deployment_dst  :: FilePath
    , deployment_kind :: DeploymentKind
    } deriving Eq

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
    parseJSON (Object o)
        = Put <$> o .: "sources"
              <*> o .: "destination"
              <*> o .: "deployment kind"
    parseJSON _ = mzero

instance ToJSON Deployment where
    toJSON depl
        = object
        [ "sources" .= deployment_srcs depl
        , "destination" .= deployment_dst depl
        , "deployment kind" .= deployment_kind depl
        ]

type CompilerPrefix = [PrefixPart]

data PrefixPart
    = Literal String
    | Alts [String]
    deriving (Show, Eq)

data CompilerState = CompilerState
    { state_deployment_kind_override :: Maybe DeploymentKind
    , state_into                     :: Directory
    , state_outof_prefix             :: CompilerPrefix
    } deriving (Show, Eq)

type PrecompileError = String

type ImpureCompiler = ExceptT CompileError (ReaderT SparkConfig IO)
type PureCompiler = ExceptT CompileError (ReaderT SparkConfig Identity)
type Precompiler = WriterT [PrecompileError] Identity
type InternalCompiler = StateT CompilerState (WriterT ([Deployment], [CardReference]) PureCompiler)

