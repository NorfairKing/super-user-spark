module Deployer where

import           Data.Either        (lefts, rights)
import           Data.Maybe         (catMaybes)
import           System.Directory   (copyFile, getPermissions)
import           System.Posix.Files (createSymbolicLink, fileExist,
                                     getFileStatus, isBlockDevice,
                                     isCharacterDevice, isDirectory,
                                     isNamedPipe, isRegularFile, isSocket,
                                     isSymbolicLink)

import           Types


deploy :: [Deployment] -> Sparker ()
deploy dp = do
    state <- initialState dp
    _ <- runSparkDeployer state deployAll
    return ()

initialState :: [Deployment] -> Sparker DeployerState
initialState dp = return $ DeployerState {
        state_deployments = dp
    ,   state_predeployments = []
    }

deployAll :: SparkDeployer ()
deployAll = do
    dry <- asks conf_dry

    predeployments
    pdps <- gets state_predeployments
    liftIO $ print pdps

    if dry
    then return ()
    else do
        deployments
        postdeployments


predeployments :: SparkDeployer ()
predeployments = do
    dps  <- gets state_deployments
    pdps <- mapM preDeployment dps
    case lefts pdps of
        [] -> modify (\s -> s { state_predeployments = rights pdps } )
        ss -> throwError $ DeployError $ PreDeployError ss

preDeployment :: Deployment -> SparkDeployer (Either String PreDeployment)
preDeployment d@(Put [] dst _) = return $ Left $ unwords ["No existing source for deployment with destination", dst]
preDeployment d@(Put (s:ss) dst kind) = do
    sd <- diagnose s
    dd <- diagnose dst
    case sd of
        NonExistent     -> preDeployment (Put ss dst kind)
        IsFile p        -> undefined
        IsDirectory p   -> undefined
        IsLink p        -> undefined
        _               -> return $ Left $ unwords ["Source", s, "is not a valid file type."]

diagnose :: FilePath -> SparkDeployer Diagnostics
diagnose fp = do
    e <- liftIO $ fileExist fp
    if not e
    then return NonExistent
    else do
        s <- liftIO $ getFileStatus fp
        if isBlockDevice s
        then return IsBlockDevice
        else if isCharacterDevice s
            then return IsCharDevice
            else if isSocket s
                then return IsSocket
                else if isNamedPipe s
                    then return IsPipe
                    else do
                        p <- liftIO $ getPermissions fp
                        if isSymbolicLink s
                        then return $ IsLink p
                        else if isDirectory s
                            then return $ IsDirectory p
                            else if isRegularFile s
                                then return $ IsFile p
                                else throwError $ UnpredictedError "Contact the author if you see this"



deployments :: SparkDeployer ()
deployments =  mapM_ deployment =<< gets state_deployments

deployment :: Deployment -> SparkDeployer ()
deployment (Put src dst kind) = do
    case kind of
        LinkDeployment -> link src dst
        CopyDeployment -> copy src dst

copy :: [FilePath] -> FilePath -> SparkDeployer ()
copy srcs dst = do
    liftIO $ copyFile (head srcs) dst

link :: [FilePath] -> FilePath -> SparkDeployer ()
link srcs dst = do
    liftIO $ createSymbolicLink (head srcs) dst


postdeployments :: SparkDeployer ()
postdeployments = do
    dps  <- gets state_predeployments
    pdps <- mapM postdeployment dps
    case catMaybes pdps of
        [] -> return ()
        es -> throwError $ DeployError $ PostDeployError es


postdeployment :: PreDeployment -> SparkDeployer (Maybe String)
postdeployment pd = return Nothing
{-
    srcd <- diagnose src
    dstd <- diagnose dst
    -- Tests
    return $ Right (src, dst, srcd, dstd) -}


