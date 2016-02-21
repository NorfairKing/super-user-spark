module Deployer.Internal where

import           Check.Types
import           Compiler.Types
import           Data.List             (isPrefixOf)
import           Data.Text             (pack)
import           Deployer.Types
import           Shelly                (cp_r, fromText, shelly)
import           System.Directory      (getHomeDirectory,
                                        removeDirectoryRecursive, removeFile)
import           System.Environment    (getEnvironment)
import           System.FilePath       (normalise, (</>))
import           System.FilePath.Posix (dropFileName)
import           System.Posix.Files    (createSymbolicLink, removeLink)
import           Types
import           Utils

completeDeployments :: [Deployment] -> IO [Deployment]
completeDeployments ds = do
    home <- getHomeDirectory
    env <- getEnvironment
    case mapM (completeDeployment home env) ds of
        Left err -> die err
        Right fp -> return fp

type Environment = [(String, String)]
type HomeDir = FilePath

completeDeployment :: HomeDir -> Environment -> Deployment -> Either String Deployment
completeDeployment home env (Put srcs dst kind) = do
    csrcs <- mapM (complete home env) srcs
    cdst <- complete home env dst
    return $ Put csrcs cdst kind

complete :: HomeDir -> Environment -> FilePath -> Either String FilePath
complete home env fp = do
    let ids = parseId fp
    strs <- mapM (replaceId env) ids
    let completed = map (replaceHome home) strs
    return $ normalise $ concat completed

parseId :: FilePath -> [ID]
parseId [] = []
parseId ('$':'(':rest) = (Var id):(parseId next)
  where (id, (')':next)) = break (\c -> c == ')') rest
parseId (s:ss) = case parseId ss of
                    (Plain str):r   -> (Plain (s:str)):r
                    r               -> (Plain [s]):r

replaceId :: Environment -> ID -> Either String FilePath
replaceId _ (Plain str) = return str
replaceId e (Var str) = do
    case lookup str e of
        Nothing -> Left $ unwords ["variable", str, "could not be resolved from environment."]
        Just fp -> Right fp

replaceHome :: HomeDir -> FilePath -> FilePath
replaceHome home path
    = if "~" `isPrefixOf` path
        then home </> drop 2 path
        else path

performClean :: CleanupInstruction -> Sparker ()
performClean (CleanFile fp)         = incase conf_deploy_replace_files       $ rmFile fp
performClean (CleanDirectory fp)    = incase conf_deploy_replace_directories $ rmDir fp
performClean (CleanLink fp)         = incase conf_deploy_replace_links       $ unlink fp

unlink :: FilePath -> Sparker ()
unlink fp = liftIO $ removeLink fp

rmFile :: FilePath -> Sparker ()
rmFile fp = liftIO $ removeFile fp

rmDir :: FilePath -> Sparker ()
rmDir fp  = liftIO $ removeDirectoryRecursive fp


performDeployment :: Instruction -> IO ()
performDeployment (Instruction source destination LinkDeployment) = link source destination
performDeployment (Instruction source destination CopyDeployment) = copy source destination

copy :: FilePath -> FilePath -> IO ()
copy src dst = do
    createDirectoryIfMissing upperDir
    shelly $ cp_r (fromText $ pack src) (fromText $ pack dst)
  where upperDir = dropFileName dst

link :: FilePath -> FilePath -> IO ()
link src dst = do
    createDirectoryIfMissing upperDir
    createSymbolicLink src dst
  where upperDir = dropFileName dst

