module Deployer.Internal where

import           Compiler.Types
import           Data.List        (isPrefixOf)
import           Deployer.Types
import           System.Directory (getHomeDirectory)
import           System.FilePath  (normalise, (</>))
import           System.Posix.Env (getEnv)

-- FIXME(kerckhove) seperate the pure part from the impure part with a Reader over the environment
completeDeployment :: Deployment -> IO Deployment
completeDeployment (Put srcs dst kind) = do
    csrcs <- mapM complete srcs
    cdst <- complete dst
    return $ Put csrcs cdst kind

complete :: FilePath -> IO FilePath
complete fp = do
    let ids = parseId fp
    strs <- mapM replaceId ids
    completed <- mapM replaceHome strs
    return $ normalise $ concat completed

parseId :: FilePath -> [ID]
parseId [] = [Plain ""]
parseId ('$':'(':rest) = (Var id):(parseId next)
  where (id, (')':next)) = break (\c -> c == ')') rest
parseId (s:ss) = case parseId ss of
                    (Plain str):r   -> (Plain (s:str)):r
                    r               -> (Plain [s]):r

replaceId :: ID -> IO FilePath
replaceId (Plain str) = return str
replaceId (Var str) = do
    e <- getEnv str
    case e of
        -- FIXME(kerckhove) make this total instead
        Nothing -> fail $ unwords ["variable", str, "could not be resolved from environment."]
        Just fp -> return fp

replaceHome :: FilePath -> IO FilePath
replaceHome path = do
    home <- getHomeDirectory
    return $ if "~" `isPrefixOf` path
        then home </> drop 2 path
        else path


