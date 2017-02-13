{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Bake.Internal where

import Import

import Control.Exception (try)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Types

bakeDeployments :: [Deployment] -> SparkBaker [BakedDeployment]
bakeDeployments = mapM bakeDeployment

bakeDeployment :: Deployment -> SparkBaker BakedDeployment
bakeDeployment Put {..} = do
    d <- bakeDirections deploymentSources deploymentDestination
    pure $ BakedDeployment {bakedDirections = d, bakedKind = deploymentKind}

bakeDirections :: [FilePath]
               -> FilePath
               -> SparkBaker (DeploymentDirections AbsP AbsP)
bakeDirections srcs dst =
    Directions <$> mapM bakeFilePath srcs <*> bakeFilePath dst

-- | Bake asingle 'FilePath'
--
-- The result should:
--
-- * ... not contain any more variables.
-- * ... not contain any reference to the home directory: @~@.
-- * ... be absolute.
bakeFilePath :: FilePath -> SparkBaker AbsP
bakeFilePath fp = do
    errOrAp <- liftIO $ try $ resolveFile' fp -- FIXME resolve from the right file.
    case errOrAp of
        Left err -> throwError $ BakeError $ show (err :: PathParseException)
        Right ap -> pure $ AbsP ap
