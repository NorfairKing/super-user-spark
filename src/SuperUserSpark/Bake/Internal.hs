{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Bake.Internal where

import Import

import Control.Exception (try)
import System.FilePath (isAbsolute)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Types

bakeDeployments :: [RawDeployment] -> SparkBaker [BakedDeployment]
bakeDeployments = mapM bakeDeployment

bakeDeployment :: RawDeployment -> SparkBaker BakedDeployment
bakeDeployment Deployment {..} = do
    d <- bakeDirections deploymentDirections
    pure $ BakedDeployment {bakedDirections = d, bakedKind = deploymentKind}

bakeDirections :: DeploymentDirections FilePath
               -> SparkBaker (DeploymentDirections AbsP)
bakeDirections (Directions srcs dst) =
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
    env <- asks bakeEnvironment
    root <- asks bakeRoot
    case complete env fp of
        Left err -> throwError $ BakeError $ err
        Right cp -> do
            if isAbsolute cp
                then case parseAbsFile cp of
                         Left err -> throwError $ BakeError $ show err
                         Right af -> pure $ AbsP af
                else do
                    errOrAp <- liftIO $ try $ resolveFile root cp
                    case errOrAp of
                        Left err ->
                            throwError $
                            BakeError $ show (err :: PathParseException)
                        Right absp -> pure $ AbsP absp

type Environment = [(String, String)]

complete :: Environment -> FilePath -> Either String FilePath
complete env fp = do
    let ids = parseId fp
    strs <- mapM (replaceId env) ids
    return $ concat strs

parseId :: FilePath -> [ID]
parseId fp =
    case fp of
        ('~':rest) -> Var "HOME" : go rest
        _ -> go fp
  where
    go :: FilePath -> [ID]
    go [] = []
    go ('$':'(':rest) = (Var id_) : (go next)
      where
        (id_, (')':next)) = break (\c -> c == ')') rest
    go (s:ss) =
        case go ss of
            (Plain str):r -> (Plain (s : str)) : r
            r -> (Plain [s]) : r

replaceId :: Environment -> ID -> Either String FilePath
replaceId _ (Plain str) = return str
replaceId e (Var str) = do
    case lookup str e of
        Nothing ->
            Left $
            unwords ["variable", str, "could not be resolved from environment."]
        Just fp -> Right fp
