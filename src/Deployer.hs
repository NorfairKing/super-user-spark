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
deploy dps = do
    state <- initialState dps
    _ <- runSparkDeployer state $ deployAll dps
    return ()

initialState :: [Deployment] -> Sparker DeployerState
initialState dp = return DeployerState

deployAll :: [Deployment] -> SparkDeployer ()
deployAll deps = do
    pdps <- predeployments deps

    dry <- asks conf_dry
    if dry
    then return ()
    else do
        deployments pdps
        postdeployments pdps


predeployments :: [Deployment] -> SparkDeployer [PreDeployment]
predeployments dps = do
    pdps <- mapM preDeployment dps
    liftIO $ putStrLn $ formatPreDeployments $ zip dps pdps
    return pdps

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
    error :: [String] -> SparkDeployer PreDeployment
    error strs = return $ Error $ unwords strs

compareFiles :: FilePath -> FilePath -> SparkDeployer Bool
compareFiles f1 f2 = do
    s1 <- liftIO $ readFile f1
    s2 <- liftIO $ readFile f2
    return $ s1 == s2

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



deployments :: [PreDeployment] -> SparkDeployer [Maybe String]
deployments = mapM deployment

deployment :: PreDeployment -> SparkDeployer (Maybe String)
deployment AlreadyDone = return Nothing
deployment (Error str) = return $ Just str
deployment (Ready src dst kind) = do
    case kind of
        LinkDeployment -> link src dst
        CopyDeployment -> copy src dst
    return Nothing

copy :: FilePath -> FilePath -> SparkDeployer ()
copy src dst = do
    liftIO $ copyFile src dst

link :: FilePath -> FilePath -> SparkDeployer ()
link src dst = do
    liftIO $ createSymbolicLink src dst


postdeployments :: [PreDeployment] -> SparkDeployer ()
postdeployments predeps = do
    pdps <- mapM postdeployment predeps
    case catMaybes pdps of
        [] -> return ()
        es -> throwError $ DeployError $ PostDeployError es


postdeployment :: PreDeployment -> SparkDeployer (Maybe String)
postdeployment AlreadyDone = return Nothing
postdeployment (Error str) = throwError $ UnpredictedError "Contact the author if you see this."
postdeployment (Ready src dst kind) = do
    sd <- diagnose src
    dd <- diagnose dst
    case sd of
        NonExistent     -> error ["The source", src, "is somehow missing after deployment"]
        IsFile      _   -> do
            case dd of
                NonExistent     -> error ["The destination", dst, "is somehow non-existent after deployment."]
                IsFile      _   -> do
                    case kind of
                        LinkDeployment -> error ["The destination", dst, "is somehow a file after a link deployment."]
                        CopyDeployment -> do
                            equal <- compareFiles src dst
                            if equal
                            then fine
                            else error ["The source and destination files are somehow still not equal"]
                IsDirectory _   -> error ["The destination", dst, "is somehow a directory after the deployment of the file", src, "."]
                IsLink      _   -> do
                    case kind of
                        CopyDeployment -> error ["The destination", dst, "is somehow a link after a copy deployment"]
                        LinkDeployment -> do
                            equal <- compareFiles src dst
                            if equal
                            then fine
                            else error ["The source and destination files are somehow still not equal"]

        IsDirectory _   -> do
            case dd of
                NonExistent     -> error ["The destination", dst, "is somehow non-existent after deployment."]
                IsFile      _   -> error ["The destination", dst, "is somehow a file after the deployment of the directory", src, "."]
                IsDirectory _   -> do
                    case kind of
                        LinkDeployment -> error ["The destination", dst, "is somehow a directory after a link deployment."]
                        CopyDeployment -> do
                            equal <- compareDirectories src dst
                            if equal
                            then fine
                            else error ["The source and destination directories are somehow still not equal"]
                IsLink      _   -> do
                    case kind of
                        CopyDeployment -> error ["The destination", dst, "is somehow a link after a copy deployment"]
                        LinkDeployment -> do
                            equal <- compareDirectories src dst
                            if equal
                            then fine
                            else error ["The source and destination directories are somehow still not equal"]

        IsLink      _   -> error ["The source", src, "is a symbolic link"]
        _               -> error ["The destination is now something weird"]

  where
    fine :: SparkDeployer (Maybe String)
    fine = return Nothing
    error :: [String] -> SparkDeployer (Maybe String)
    error err = return $ Just $ unwords err


{-
    srcd <- diagnose src
    dstd <- diagnose dst
    -- Tests
    return $ Right (src, dst, srcd, dstd) -}


