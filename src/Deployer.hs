module Deployer where

import           System.Directory   (copyFile)
import           System.Posix.Files (createSymbolicLink)

import           Types


deploy :: [Deployment] -> Sparker ()
deploy dp = do
    dry <- asks conf_dry
    if dry
    then return ()
    else do
        state <- initialState dp
        runSparkDeployer state deployAll
        return ()

initialState :: [Deployment] -> Sparker DeployerState
initialState dp = return $ DeployerState {
        state_deployments_left = dp
    }

deployAll :: SparkDeployer ()
deployAll = do
    d <- done
    if d
    then return ()
    else oneDeployment >> deployAll

pop :: SparkDeployer Deployment
pop = do
    deploymentsLeft <- gets state_deployments_left
    modify (\s -> s {state_deployments_left = tail deploymentsLeft})
    return $ head deploymentsLeft

done :: SparkDeployer Bool
done = fmap null $ gets state_deployments_left

oneDeployment :: SparkDeployer ()
oneDeployment = do
    dp <- pop
    case dp of
        (Link src dst) -> link src dst
        (Copy src dst) -> copy src dst




copy :: FilePath -> FilePath -> SparkDeployer ()
copy src dst = do
    liftIO $ copyFile src dst

link :: FilePath -> FilePath -> SparkDeployer ()
link src dst = do
    liftIO $ createSymbolicLink src dst




