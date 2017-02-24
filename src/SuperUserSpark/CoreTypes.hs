{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.CoreTypes where

import Import

import Control.Monad (mzero)
import Data.Aeson

type Directory = FilePath

-- | The kind of a deployment
data DeploymentKind
    = LinkDeployment
    | CopyDeployment
    | PipeDeployment String
    deriving (Show, Eq, Generic)

instance Validity DeploymentKind

instance Read DeploymentKind where
    readsPrec _ "link" = [(LinkDeployment, "")]
    readsPrec _ "copy" = [(CopyDeployment, "")]
    readsPrec _ _ = []

instance FromJSON DeploymentKind where
    parseJSON (String "link") = return LinkDeployment
    parseJSON (String "copy") = return CopyDeployment
    parseJSON (Object o) = do
        k <- o .: "kind"
        case k of
            String "pipe" -> PipeDeployment <$> o .: "command"
            _ -> mempty
    parseJSON _ = mzero

instance ToJSON DeploymentKind where
    toJSON LinkDeployment = String "link"
    toJSON CopyDeployment = String "copy"
    toJSON (PipeDeployment c) =
        object ["kind" .= ("pipe" :: String), "command" .= c]
