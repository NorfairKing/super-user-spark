{-# LANGUAGE OverloadedStrings #-}

module Deployer where

import           Check
import           Check.Internal
import           Check.Types
import           Compiler.Types
import           Control.Monad         (forM_, when)
import           Data.Text             (pack)
import           Monad
import           Prelude               hiding (error)
import           Shelly                (cp_r, fromText, shelly)
import           System.Directory      (createDirectoryIfMissing,
                                        removeDirectoryRecursive, removeFile)
import           System.FilePath.Posix (dropFileName)
import           System.Posix.Files    (createSymbolicLink, removeLink)
import           Types
import           Utils


deploy :: [(Deployment, DeploymentCheckResult)] -> Sparker ()
deploy dcrs = do
    let crs = map snd dcrs
    -- Check for impossible deployments
    when (any impossibleDeployment crs) $
        err dcrs "Deployment is impossible."

    -- Clean up the situation
    forM_ crs $ \d -> do
        case d of
            DirtySituation _ _ cis -> performClean cis
            _ -> return ()

    -- Check again
    let ds = map fst dcrs
    dcrs2 <- liftIO $ check ds

    -- Error if the cleaning is not done now.
    when (any (impossibleDeployment ||| dirtyDeployment) dcrs2) $
        err (zip ds dcrs2) "Situation was not entirely clean after attemted cleanup. Maybe you forgot to enable cleanups (--replace-all)?"

    -- Perform deployments
    liftIO $ mapM_ performDeployment $ map (\(ReadyToDeploy i) -> i) $ filter deploymentReadyToDeploy dcrs2

    -- Check one last time.
    dcrsf <- liftIO $ check ds
    when (any (not . deploymentIsDone) dcrsf) $ do
        err (zip ds dcrsf) "Something went wrong during deployment. It's not done yet."
  where
    err :: [(Deployment, DeploymentCheckResult)] -> String -> Sparker ()
    err dcrs text = do
        liftIO $ putStrLn $ formatDeploymentChecks dcrs
        throwError $ DeployError text




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
    createDirectoryIfMissing True upperDir
    shelly $ cp_r (fromText $ pack src) (fromText $ pack dst)
  where upperDir = dropFileName dst

link :: FilePath -> FilePath -> IO ()
link src dst = do
    createDirectoryIfMissing True upperDir
    createSymbolicLink src dst
  where upperDir = dropFileName dst
