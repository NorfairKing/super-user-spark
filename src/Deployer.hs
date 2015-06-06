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
    left <- gets state_deployments_left
    modify (\s -> s {state_deployments_left = tail left})
    return $ head left

done :: SparkDeployer Bool
done = fmap null $ gets state_deployments_left

oneDeployment :: SparkDeployer ()
oneDeployment = do
    dp <- pop
    case dp of
        (Link src dst) -> liftIO $ createSymbolicLink src dst
        (Copy src dst) -> liftIO $ copyFile src dst

formatDeployments :: [Deployment] -> String
formatDeployments = unlines . map formatDeployment

formatDeployment :: Deployment -> String
formatDeployment (Link src dst) = unwords [src, "l->", dst]
formatDeployment (Copy src dst) = unwords [src, "c->", dst]

