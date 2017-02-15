{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Compiler.Types where

import Import

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.:), (.=), withObject)

import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types
import SuperUserSpark.Parser.Types
import SuperUserSpark.PreCompiler.Types

data CompileAssignment = CompileAssignment
    { compileCardReference :: StrongCardFileReference
    , compileSettings :: CompileSettings
    } deriving (Show, Eq, Generic)

instance Validity CompileAssignment

data StrongCardFileReference =
    StrongCardFileReference (Path Abs File)
                            (Maybe CardNameReference)
    deriving (Show, Eq, Generic)

instance Validity StrongCardFileReference

data StrongCardReference
    = StrongCardFile StrongCardFileReference
    | StrongCardName CardNameReference
    deriving (Show, Eq, Generic)

instance Validity StrongCardReference

data CompileSettings = CompileSettings
    { compileOutput :: Maybe (Path Abs File)
    , compileDefaultKind :: DeploymentKind
    , compileKindOverride :: Maybe DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity CompileSettings

defaultCompileSettings :: CompileSettings
defaultCompileSettings =
    CompileSettings
    { compileOutput = Nothing
    , compileDefaultKind = LinkDeployment
    , compileKindOverride = Nothing
    }

type RawDeployment = Deployment FilePath

data Deployment a = Deployment
    { deploymentDirections :: DeploymentDirections a
    , deploymentKind :: DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity a =>
         Validity (Deployment a)

instance FromJSON a =>
         FromJSON (Deployment a) where
    parseJSON =
        withObject "Deployment" $ \o ->
            Deployment <$> o .: "directions" <*> o .: "deployment kind"

instance ToJSON a =>
         ToJSON (Deployment a) where
    toJSON depl =
        object
            [ "directions" .= deploymentDirections depl
            , "deployment kind" .= deploymentKind depl
            ]

instance Functor Deployment where
    fmap f (Deployment dis dk) = Deployment (fmap f dis) dk

data DeploymentDirections a = Directions
    { directionSources :: [a]
    , directionDestination :: a
    } deriving (Show, Eq, Generic)

instance Validity a =>
         Validity (DeploymentDirections a)

instance ToJSON a =>
         ToJSON (DeploymentDirections a) where
    toJSON (Directions srcs dst) =
        object ["sources" .= srcs, "destination" .= dst]

instance FromJSON a =>
         FromJSON (DeploymentDirections a) where
    parseJSON =
        withObject "Deployment Directions" $ \o ->
            Directions <$> o .: "sources" <*> o .: "destination"

instance Functor DeploymentDirections where
    fmap f (Directions srcs dst) = Directions (map f srcs) (f dst)

type CompilerPrefix = [PrefixPart]

data PrefixPart
    = Literal String
    | Alts [String]
    deriving (Show, Eq, Generic)

instance Validity PrefixPart

data CompilerState = CompilerState
    { stateDeploymentKindLocalOverride :: Maybe DeploymentKind
    , stateInto :: Directory
    , stateOutof_prefix :: CompilerPrefix
    } deriving (Show, Eq, Generic)

instance Validity CompilerState

type ImpureCompiler = ExceptT CompileError (ReaderT CompileSettings IO)

type PureCompiler = ExceptT CompileError (ReaderT CompileSettings Identity)

type InternalCompiler = StateT CompilerState (WriterT ([RawDeployment], [CardReference]) PureCompiler)

data CompileError
    = CompileParseError ParseError
    | PreCompileErrors [PreCompileError]
    | DuringCompilationError String
    deriving (Show, Eq, Generic)

instance Validity CompileError
