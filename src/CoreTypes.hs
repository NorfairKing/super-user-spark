{-# LANGUAGE OverloadedStrings #-}
module CoreTypes where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON (..), ToJSON (..), Value (..))

import           Data.Binary   (Binary (..), Get)
import qualified Data.Binary   as B

-- | The kind of a deployment
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

