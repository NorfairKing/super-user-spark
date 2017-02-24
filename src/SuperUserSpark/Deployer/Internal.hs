module SuperUserSpark.Deployer.Internal where

import Import hiding ((</>))

import Data.Text (pack)
import System.FilePath.Posix
       (dropFileName, dropTrailingPathSeparator)
import System.IO
import System.Posix.Files (createSymbolicLink, removeLink)
import System.Process

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Utils

performClean :: CleanupInstruction -> SparkDeployer ()
performClean (CleanFile fp) = incase deploySetsReplaceFiles $ rmFile fp
performClean (CleanDirectory fp) =
    incase deploySetsReplaceDirectories $ rmDir fp
performClean (CleanLink fp) = incase deploySetsReplaceLinks $ unlink fp

unlink :: Path Abs File -> SparkDeployer ()
unlink = liftIO . removeLink . dropTrailingPathSeparator . toFilePath

rmFile :: Path Abs File -> SparkDeployer ()
rmFile = liftIO . removeFile

rmDir :: Path Abs Dir -> SparkDeployer ()
rmDir = liftIO . removeDirRecur

performDeployment :: Instruction -> IO ()
performDeployment (CopyFile source destination) =
    performCopyFile source destination
performDeployment (CopyDir source destination) =
    performCopyDir source destination
performDeployment (LinkFile source destination) =
    performLinkFile source destination
performDeployment (LinkDir source destination) =
    performLinkDir source destination
performDeployment (PipeFile command source destination) =
    performPipeFile command source destination

performCopyFile :: Path Abs File -> Path Abs File -> IO ()
performCopyFile src dst = do
    ensureDir $ parent dst
    copyFile src dst

performCopyDir :: Path Abs Dir -> Path Abs Dir -> IO ()
performCopyDir src dst = do
    ensureDir $ parent dst
    copyDirRecur src dst

performLinkFile :: Path Abs File -> Path Abs File -> IO ()
performLinkFile src dst = do
    ensureDir $ parent dst
    createSymbolicLink (toFilePath src) (toFilePath dst)

performLinkDir :: Path Abs Dir -> Path Abs Dir -> IO ()
performLinkDir src dst = do
    ensureDir $ parent dst
    createSymbolicLink
        (dropTrailingPathSeparator $ toFilePath src)
        (dropTrailingPathSeparator $ toFilePath dst)

performPipeFile :: String -> Path Abs File -> Path Abs File -> IO ()
performPipeFile command src dst =
    withSystemTempDir "super-user-spark" $ \dir ->
        withFile (toFilePath src) ReadMode $ \srch ->
            withFile (toFilePath dst) WriteMode $ \dsth -> do
                let cp =
                        (shell $ unwords ["bash -c", command])
                        { cwd = Just $ toFilePath dir
                        , std_in = UseHandle srch
                        , std_out = UseHandle dsth
                        , std_err = Inherit
                        }
                (Nothing, Nothing, Nothing, ph) <- createProcess cp
                ec <- waitForProcess ph
                case ec of
                    ExitSuccess -> pure ()
                    ExitFailure i ->
                        fail $
                        unlines
                            [ "Deployment of source"
                            , toFilePath src
                            , "to destination"
                            , toFilePath dst
                            , "using a pipe deployment with command"
                            , show command
                            , "failed with exit code"
                            , show i
                            ]
