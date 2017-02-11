module SuperUserSpark.Deployer.Internal where

import Import hiding ((</>), removeFile)

import Data.List (isPrefixOf)
import Data.Text (pack)
import Shelly (cp_r, fromText, shelly)
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Utils
import System.Directory
       (getHomeDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment)
import System.FilePath (normalise, (</>))
import System.FilePath.Posix (dropFileName)
import System.Posix.Files (createSymbolicLink, removeLink)

completeDeployments :: [Deployment] -> IO [Deployment]
completeDeployments ds = do
    home <- getHomeDirectory
    env <- getEnvironment
    case mapM (completeDeployment home env) ds of
        Left err -> die err
        Right fp -> return fp

type Environment = [(String, String)]

type HomeDir = FilePath

completeDeployment :: HomeDir
                   -> Environment
                   -> Deployment
                   -> Either String Deployment
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
parseId ('$':'(':rest) = (Var id_) : (parseId next)
  where
    (id_, (')':next)) = break (\c -> c == ')') rest
parseId (s:ss) =
    case parseId ss of
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

replaceHome :: HomeDir -> FilePath -> FilePath
replaceHome home path =
    if "~" `isPrefixOf` path
        then home </> drop 2 path
        else path

performClean :: CleanupInstruction -> SparkDeployer ()
performClean (CleanFile fp) = incase deploySetsReplaceFiles $ rmFile fp
performClean (CleanDirectory fp) =
    incase deploySetsReplaceDirectories $ rmDir fp
performClean (CleanLink fp) = incase deploySetsReplaceLinks $ unlink fp

unlink :: FilePath -> SparkDeployer ()
unlink = liftIO . removeLink

rmFile :: FilePath -> SparkDeployer ()
rmFile = liftIO . removeFile

rmDir :: FilePath -> SparkDeployer ()
rmDir = liftIO . removeDirectoryRecursive

performDeployment :: Instruction -> IO ()
performDeployment (Instruction source destination LinkDeployment) =
    link source destination
performDeployment (Instruction source destination CopyDeployment) =
    copy source destination

copy :: FilePath -> FilePath -> IO ()
copy src dst = do
    createDirectoryIfMissing upperDir
    shelly $ cp_r (fromText $ pack src) (fromText $ pack dst)
  where
    upperDir = dropFileName dst

link :: FilePath -> FilePath -> IO ()
link src dst = do
    createDirectoryIfMissing upperDir
    createSymbolicLink src dst
  where
    upperDir = dropFileName dst
