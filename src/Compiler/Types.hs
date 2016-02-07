{-# LANGUAGE OverloadedStrings #-}
module Compiler.Types where

import           Control.Monad         (mzero)

import           Data.Aeson            (FromJSON (..), ToJSON (..), Value (..),
                                        object, (.:), (.=))


import           Data.Binary           (Binary (..), Get)
import qualified Data.Binary           as B

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack, unpack)

import           Constants
import           CoreTypes
import           Language.Types
import           Monad
import           Types

data Deployment = Put
    {   deployment_srcs :: [FilePath]
    ,   deployment_dst  :: FilePath
    ,   deployment_kind :: DeploymentKind
    } deriving Eq

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

type CompilerPrefix = [PrefixPart]

data PrefixPart = Literal String
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

