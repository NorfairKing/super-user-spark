module SuperUserSpark.Deployer.Internal where

import Import hiding ((</>), removeFile)

import Data.List (isPrefixOf)
import Data.Text (pack)
import Shelly (cp_r, fromText, shelly)
import System.Directory
       (getHomeDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment)
import System.FilePath (normalise, (</>))
import System.FilePath.Posix (dropFileName)
import System.Posix.Files (createSymbolicLink, removeLink)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Utils

performClean :: CleanupInstruction -> SparkDeployer ()
performClean (CleanFile fp) = incase deploySetsReplaceFiles $ rmFile fp
performClean (CleanDirectory fp) =
    incase deploySetsReplaceDirectories $ rmDir fp
performClean (CleanLink fp) = incase deploySetsReplaceLinks $ unlink fp

unlink :: AbsP -> SparkDeployer ()
unlink = liftIO . removeLink . toPath

rmFile :: AbsP -> SparkDeployer ()
rmFile = liftIO . removeFile . toPath

rmDir :: AbsP -> SparkDeployer ()
rmDir = liftIO . removeDirectoryRecursive . toPath

performDeployment :: Instruction -> IO ()
performDeployment (Instruction source destination LinkDeployment) =
    link source destination
performDeployment (Instruction source destination CopyDeployment) =
    copy source destination

copy :: AbsP -> AbsP -> IO ()
copy src dst = do
    createDirectoryIfMissing upperDir
    shelly $ cp_r (fromText $ pack $ toPath src) (fromText $ pack $ toPath dst)
  where
    upperDir = dropFileName $ toPath dst

link :: AbsP -> AbsP -> IO ()
link src dst = do
    createDirectoryIfMissing upperDir
    createSymbolicLink (toPath src) (toPath dst)
  where
    upperDir = dropFileName (toPath dst)
