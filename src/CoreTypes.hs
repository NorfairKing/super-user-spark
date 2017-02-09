{-# LANGUAGE OverloadedStrings #-}

module CoreTypes where

import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))

type Directory = FilePath

-- | The kind of a deployment
data DeploymentKind
    = LinkDeployment
    | CopyDeployment
    deriving (Show, Eq)

instance Read DeploymentKind where
    readsPrec _ "link" = [(LinkDeployment, "")]
    readsPrec _ "copy" = [(CopyDeployment, "")]
    readsPrec _ _ = []

instance FromJSON DeploymentKind where
    parseJSON (String "link") = return LinkDeployment
    parseJSON (String "copy") = return CopyDeployment
    parseJSON _ = mzero

instance ToJSON DeploymentKind where
    toJSON LinkDeployment = String "link"
    toJSON CopyDeployment = String "copy"
