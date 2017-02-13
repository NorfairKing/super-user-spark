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
