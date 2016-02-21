module Deployer.Internal where

import           Check.Types
import           Compiler.Types
import           Data.List             (isPrefixOf)
import           Data.Text             (pack)
import           Deployer.Types
import           Shelly                (cp_r, fromText, shelly)
import           System.Directory      (getHomeDirectory,
                                        removeDirectoryRecursive, removeFile)
import           System.FilePath       (normalise, (</>))
import           System.FilePath.Posix (dropFileName)
import           System.Posix.Env      (getEnv)
import           System.Posix.Files    (createSymbolicLink, removeLink)
import           Types
import           Utils

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

