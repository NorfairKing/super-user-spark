{-# LANGUAGE OverloadedStrings #-}

module Deployer where

import           Data.Either        (lefts, rights)
import           Data.Maybe         (catMaybes)
import           Data.Text          (pack)
import           Shelly             (cp_r, fromText, shelly)
import           System.Directory   (copyFile, createDirectoryIfMissing,
                                     getDirectoryContents, getPermissions)
import           System.FilePath    (dropFileName)
import           System.Posix.Files (createSymbolicLink, fileExist,
                                     getFileStatus, isBlockDevice,
                                     isCharacterDevice, isDirectory,
                                     isNamedPipe, isRegularFile, isSocket,
                                     isSymbolicLink)

import           Formatter          (formatPostDeployments,
                                     formatPreDeployments)
import           Types
import           Utils


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

    case catErrors pdps of
        [] -> do
            dry <- asks conf_dry
            if dry
            then return ()
            else do
                deployments pdps
                postdeployments deps pdps

        ss -> throwError $ DeployError $ PreDeployError ss

catErrors :: [PreDeployment] -> [String]
catErrors [] = []
catErrors (s:ss) = case s of
                    Ready _ _ _ -> catErrors ss
                    AlreadyDone -> catErrors ss
                    Error str   -> str : catErrors ss


predeployments :: [Deployment] -> SparkDeployer [PreDeployment]
predeployments dps = do
    pdps <- mapM preDeployment dps
    lift $ verboseOrDry $ formatPreDeployments $ zip dps pdps
    return pdps

preDeployment :: Deployment -> SparkDeployer PreDeployment
preDeployment d@(Put [] dst _) = return $ Error $ unwords ["No source for deployment with destination:", dst]
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
                    else error ["Destination", dst, "already exists and is a file, different from the source:", s, "."]
                IsDirectory _   -> error ["Destination", dst, "already exists and is a directory."]
                IsLink      _   -> error ["Destination", dst, "already exists and is a symbolic link."]
                _               -> error ["Destination", dst, "already exists and is something weird."]
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


cmpare :: FilePath -> FilePath -> SparkDeployer Bool
cmpare f1 f2 = do
    d1 <- diagnose f1
    d2 <- diagnose f2
    if d1 /= d2
    then return False
    else case d1 of
        IsFile      _   -> compareFiles f1 f2
        IsDirectory _   -> compareDirectories f1 f2
        _           -> return True

compareFiles :: FilePath -> FilePath -> SparkDeployer Bool
compareFiles f1 f2 = do
    s1 <- liftIO $ readFile f1
    s2 <- liftIO $ readFile f2
    return $ s1 == s2

compareDirectories :: FilePath -> FilePath -> SparkDeployer Bool
compareDirectories d1 d2 = do
    dc1 <- contents d1
    dc2 <- contents d2
    b <- mapM (uncurry cmpare) $ zip dc1 dc2
    return $ and b
  where
    contents d = do
        cs <- liftIO $ getDirectoryContents d
        return $ filter (\f -> not $ f == "." || f == "..") cs

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
    liftIO $ createDirectoryIfMissing True upperDir
    liftIO $ shelly $ cp_r (fromText $ pack src) (fromText $ pack dst)
  where upperDir = dropFileName dst

link :: FilePath -> FilePath -> SparkDeployer ()
link src dst = do
    liftIO $ createDirectoryIfMissing True upperDir
    liftIO $ createSymbolicLink src dst
  where upperDir = dropFileName dst


postdeployments :: [Deployment] -> [PreDeployment] -> SparkDeployer ()
postdeployments deps predeps = do
    pdps <- mapM postdeployment predeps
    lift $ verbose $ formatPostDeployments $ zip deps pdps
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
                    equal <- compareFiles src dst
                    if equal
                    then fine
                    else error ["The source and destination files are somehow still not equal"]
                IsDirectory _   -> error ["The destination", dst, "is somehow a directory after the deployment of the file", src, "."]
                IsLink      _   -> error ["The destination", dst, "is somehow a link after deployment"]
        IsDirectory _   -> do
            case dd of
                NonExistent     -> error ["The destination", dst, "is somehow non-existent after deployment."]
                IsFile      _   -> error ["The destination", dst, "is somehow a file after the deployment of the directory", src, "."]
                IsDirectory _   -> do
                    equal <- compareDirectories src dst
                    if equal
                    then fine
                    else error ["The source and destination directories are somehow still not equal"]
                IsLink      _   -> error ["The destination", dst, "is somehow a link after deployment"]

        IsLink      _   -> error ["The source", src, "is a symbolic link"]
        _               -> error ["The destination is now something weird"]

  where
    fine :: SparkDeployer (Maybe String)
    fine = return Nothing
    error :: [String] -> SparkDeployer (Maybe String)
    error err = return $ Just $ unwords err

