{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Bake.Types where

import Import

import System.FilePath (takeExtension)

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Types

data BakeAssignment = BakeAssignment
    { bakeCardReference :: BakeCardReference
    , bakeSettings :: BakeSettings
    } deriving (Show, Eq, Generic)

instance Validity BakeAssignment

data BakeCardReference
    = BakeCardCompiled FilePath -- TODO make absolute when we do something better than 'Read'.
    | BakeCardUncompiled CardFileReference
    deriving (Show, Eq, Generic)

instance Validity BakeCardReference

instance Read BakeCardReference where
    readsPrec _ fp =
        case length (words fp) of
            0 -> []
            1 ->
                if takeExtension fp == ".sus"
                    then [ ( BakeCardUncompiled (CardFileReference fp Nothing)
                           , "")
                         ]
                    else [(BakeCardCompiled fp, "")]
            2 ->
                let [f, c] = words fp
                in [ ( BakeCardUncompiled
                           (CardFileReference f (Just $ CardNameReference c))
                     , "")
                   ]
            _ -> []

data BakeSettings = BakeSettings
    { bakeCompileSettings :: CompileSettings
    } deriving (Show, Eq, Generic)

instance Validity BakeSettings

type SparkBaker = ExceptT BakeError (ReaderT BakeSettings IO)

data BakeError
    = BakeCompileError CompileError
    | BakeError String
    deriving (Show, Eq, Generic)

instance Validity BakeError

data BakedDeployment
    = CopyFile (DeploymentDirections (Path Abs File) (Path Abs File))
    | CopyDir (DeploymentDirections (Path Abs Dir) (Path Abs Dir))
    | LinkFile (DeploymentDirections (Path Abs File) (Path Abs File))
    | LinkDir (DeploymentDirections (Path Abs Dir) (Path Abs File))
    deriving (Show, Eq, Generic)

instance Validity BakedDeployment

data DeploymentDirections a b = Directions
    { directionSources :: [a]
    , directionDestination :: b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) =>
         Validity (DeploymentDirections a b)
