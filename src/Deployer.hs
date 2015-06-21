module Deployer where

import           Data.Either        (lefts, rights)
import           Data.Maybe         (catMaybes)
import           System.Directory   (copyFile, getPermissions)
import           System.Posix.Files (createSymbolicLink, fileExist,
                                     getFileStatus, isBlockDevice,
                                     isCharacterDevice, isDirectory,
                                     isNamedPipe, isRegularFile, isSocket,
                                     isSymbolicLink)

import           Formatter          (formatPreDeployments)
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
    liftIO $ putStrLn $ formatPreDeployments $ zip dps pdps
    {-case lefts pdps of
        [] -> modify (\s -> s { state_predeployments = rights pdps } )
        ss -> throwError $ DeployError $ PreDeployError ss-}

preDeployment :: Deployment -> SparkDeployer PreDeployment
preDeployment d@(Put [] dst _) = return $ Error $ unwords ["No existing source for deployment with destination", dst]
preDeployment d@(Put (s:ss) dst kind) = do
    sd <- diagnose s
    dd <- diagnose dst
    case sd of
        NonExistent     -> preDeployment (Put ss dst kind)
        IsFile p        -> do
            case dd of
                NonExistent     -> ready
                IsFile      _   -> do
                    equal <- compareFiles s dst
                    if equal
                    then done
                    else error ["Destination", dst, "already exists and is a file."]
                IsDirectory _   -> error ["Destination", dst, "already exists and is a directory."]
                IsLink      _   -> error ["destination", dst, "already exists and is a symbolic link."]
                _               -> error ["destination", dst, "already exists and is something weird."]
        IsDirectory p   -> do
            case dd of
                NonExistent     -> ready
                IsFile      _   -> error ["Destination", dst, "already exists and is a directory."]
                IsDirectory _   -> do
                    equal <- compareDirectories s dst
                    if equal
                    then done
                    else error ["Destination", dst, "already exists and is a directory."]
                IsLink      _   -> error ["destination", dst, "already exists and is a symbolic link."]
                _               -> error ["destination", dst, "already exists and is something weird."]
        IsLink p        -> error ["Source", s, "is a symbolic link."]
        _               -> error ["Source", s, "is not a valid file type."]

  where
    done :: SparkDeployer PreDeployment
    done = return $ AlreadyDone
    ready :: SparkDeployer PreDeployment
    ready = return $ Ready s dst kind
    warning :: [String] -> SparkDeployer PreDeployment
    warning strs = return $ Warning $ unwords strs
    error :: [String] -> SparkDeployer PreDeployment
    error strs = return $ Error $ unwords strs

compareFiles :: FilePath -> FilePath -> SparkDeployer Bool
compareFiles f1 f2 = return False

compareDirectories :: FilePath -> FilePath -> SparkDeployer Bool
compareDirectories f1 f2 = return False


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


