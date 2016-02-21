{-# LANGUAGE OverloadedStrings #-}

module Deployer where

import           Check.Types
import           Monad
import           Prelude            hiding (error)
import           System.Directory   (removeDirectoryRecursive, removeFile)
import           System.Posix.Files (removeLink)
import           Types
import           Utils


deploy :: [DeploymentCheckResult] -> Sparker [DeploymentCheckResult]
deploy = undefined


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

{-

deploy :: [Deployment] -> Sparker ()
deploy deps = do
    pdps <- predeployments deps

    case catErrors pdps of
        [] -> do
            void $ deployments pdps
            postdeployments deps pdps

        ss -> throwError $ DeployError $ PreDeployError ss

catErrors :: [PreDeployment] -> [String]
catErrors [] = []
catErrors (s:ss) = case s of
                    Ready _ _ _ -> catErrors ss
                    AlreadyDone -> catErrors ss
                    Error str   -> str : catErrors ss


predeployments :: [Deployment] -> Sparker [PreDeployment]
predeployments dps = do
    pdps <- mapM preDeployment dps
    lift $ debug $ formatPreDeployments $ zip dps pdps
    return pdps


preDeployment :: Deployment -> Sparker PreDeployment
preDeployment (Put [] dst _) = return $ Error $ unwords ["No source for deployment with destination:", dst]
preDeployment dep@(Put (src:ss) dst kind) = do
    s  <- liftIO $ complete src
    d  <- liftIO $ complete dst
    sd <- diagnose s
    dd <- diagnose d

    let ready = return (Ready s d kind) :: Sparker PreDeployment

    case (sd, dd, kind) of
        (NonExistent    , _             , _             )   -> preDeployment (Put ss d kind)
        (IsFile _       , NonExistent   , _             )   -> ready
        (IsFile _       , IsFile _      , LinkDeployment)   -> incaseElse conf_deploy_replace_files
                                                                (rmFile d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is file (for a link deployment):", s, "."])
        (IsFile _       , IsFile _      , CopyDeployment)   -> do
                                                                equal <- compareFiles s d
                                                                if equal
                                                                then done
                                                                else do
                                                                    incaseElse conf_deploy_replace_files
                                                                        (rmFile d >> preDeployment dep)
                                                                        (error ["Destination", d, "already exists and is a file, different from the source:", s, "."])
        (IsFile _       , IsDirectory _ , _             )   -> incaseElse conf_deploy_replace_directories
                                                                (rmDir d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is a directory."])
        (IsFile _       , IsLink _      , LinkDeployment)   -> do
                                                                point <- liftIO $ readSymbolicLink d
                                                                if point `filePathEqual` s
                                                                then done
                                                                else do
                                                                    liftIO $ putStrLn $ d ++ " is a link but it points to " ++ point ++ " instead of " ++ src
                                                                    incaseElse conf_deploy_replace_links
                                                                        (unlink d >> preDeployment dep)
                                                                        (error ["Destination", d, "already exists and is a symbolic link but not to the source."])
        (IsFile _       , IsLink _      , CopyDeployment)   -> incaseElse conf_deploy_replace_links
                                                                (unlink d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is a symbolic link (for a copy deployment):", s, "."])
        (IsFile _       , _             , _             )   -> error ["Destination", d, "already exists and is something weird."]
        (IsDirectory _  , NonExistent   , _             )   -> ready
        (IsDirectory _  , IsFile _      , _             )   -> incaseElse conf_deploy_replace_files
                                                                (rmFile d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is a file."])
        (IsDirectory _  , IsDirectory _ , LinkDeployment)   -> incaseElse conf_deploy_replace_directories
                                                                (rmDir d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is directory (for a link deployment):", s, "."])
        (IsDirectory _  , IsDirectory _ , CopyDeployment)   -> do
                                                                equal <- compareDirectories s d
                                                                if equal
                                                                then done
                                                                else do
                                                                    incaseElse conf_deploy_replace_directories
                                                                        (rmDir d >> preDeployment dep)
                                                                        (error ["Destination", d, "already exists and is a directory, different from the source."])
        (IsDirectory _  , IsLink _      , LinkDeployment)   -> do
                                                                point <- liftIO $ readSymbolicLink d
                                                                if point `filePathEqual` s
                                                                then done
                                                                else incaseElse conf_deploy_replace_links
                                                                    (unlink d >> preDeployment dep)
                                                                    (error ["Destination", d, "already exists and is a symbolic link but not to the source."])
        (IsDirectory _  , IsLink _      , CopyDeployment)   -> incaseElse conf_deploy_replace_links
                                                                (unlink d >> preDeployment dep)
                                                                (error ["Destination", d, "already exists and is a symbolic link."])
        (IsLink _       , _             , _             )   -> error ["Source", s, "is a symbolic link."]
        _                                                   -> error ["Source", s, "is not a valid file type."]


  where
    done :: Sparker PreDeployment
    done = return $ AlreadyDone


    error :: [String] -> Sparker PreDeployment
    error strs = return $ Error $ unwords strs


cmpare :: FilePath -> FilePath -> Sparker Bool
cmpare f1 f2 = do
    d1 <- diagnose f1
    d2 <- diagnose f2
    if d1 /= d2
    then return False
    else case d1 of
        IsFile      _   -> compareFiles f1 f2
        IsDirectory _   -> compareDirectories f1 f2
        _           -> return True

compareFiles :: FilePath -> FilePath -> Sparker Bool
compareFiles f1 f2 = do
    s1 <- liftIO $ readFile f1
    s2 <- liftIO $ readFile f2
    return $ s1 == s2

compareDirectories :: FilePath -> FilePath -> Sparker Bool
compareDirectories d1 d2 = do
    dc1 <- contents d1
    dc2 <- contents d2
    b <- mapM (uncurry cmpare) $ zip dc1 dc2
    return $ and b
  where
    contents d = do
        cs <- liftIO $ getDirectoryContents d
        return $ filter (\f -> not $ f == "." || f == "..") cs

deployments :: [PreDeployment] -> Sparker [Maybe String]
deployments = mapM deployment

deployment :: PreDeployment -> Sparker (Maybe String)
deployment AlreadyDone = return Nothing
deployment (Error str) = return $ Just str
deployment (Ready src dst kind) = do
    case kind of
        LinkDeployment -> link src dst
        CopyDeployment -> copy src dst
    return Nothing

copy :: FilePath -> FilePath -> Sparker ()
copy src dst = do
    debug $ unwords ["Copying:", src, "c->", dst]
    liftIO $ createDirectoryIfMissing True upperDir
    liftIO $ shelly $ cp_r (fromText $ pack src) (fromText $ pack dst)
  where upperDir = dropFileName dst

link :: FilePath -> FilePath -> Sparker ()
link src dst = do
    debug $ unwords ["Linking:", src, "l->", dst]
    liftIO $ createDirectoryIfMissing True upperDir
    liftIO $ createSymbolicLink src dst
  where upperDir = dropFileName dst

postdeployments :: [Deployment] -> [PreDeployment] -> Sparker ()
postdeployments deps predeps = do
    pdps <- mapM postdeployment predeps
    lift $ debug $ formatPostDeployments $ zip deps pdps
    case catMaybes pdps of
        [] -> return ()
        es -> throwError $ DeployError $ PostDeployError es

postdeployment :: PreDeployment -> Sparker (Maybe String)
postdeployment AlreadyDone = return Nothing
postdeployment (Error _) = throwError $ UnpredictedError "Contact the author if you see this. (postdeployment)"
postdeployment (Ready s d kind) = do
    sd <- diagnoseFp src
    dd <- diagnoseFp dst

    case (sd, dd, kind) of
        (NonExistent    , _             , _             ) -> error ["The source", src, "is somehow missing after deployment."]
        (IsFile _       , NonExistent   , _             ) -> error ["The destination", dst, "is somehow non-existent after deployment."]
        (IsFile _       , IsFile _      , LinkDeployment) -> error ["The destination", dst, "is somehow a file while it was a link deployment."]
        (IsFile _       , IsFile _      , CopyDeployment) -> compareFile
        (IsFile _       , IsDirectory _ , _             ) -> error ["The destination", dst, "is somehow a directory after the deployment of the file", src, "."]
        (IsFile _       , IsLink _      , LinkDeployment) -> compareLink
        (IsFile _       , IsLink _      , CopyDeployment) -> error ["The destination", dst, "is somehow a link while it was a copy deployment."]
        (IsFile _       , _             , _             ) -> error ["The destination", dst, "is something weird after deployment."]
        (IsDirectory _  , NonExistent   , _             ) -> error ["The destination", dst, "is somehow non-existent after deployment."]
        (IsDirectory _  , IsFile _      , _             ) -> error ["The destination", dst, "is somehow a file after the deployment of the directory", src, "."]
        (IsDirectory _  , IsDirectory _ , LinkDeployment) -> error ["The destination", dst, "is somehow a directory while it was a link deployment."]
        (IsDirectory _  , IsDirectory _ , CopyDeployment) -> compareDir
        (IsDirectory _  , IsLink _      , LinkDeployment) -> compareLink
        (IsDirectory _  , IsLink _      , CopyDeployment) -> error ["The destination", dst, "is somehow a link while it was a copy deployment."]
        (IsDirectory _  , _             , _             ) -> error ["The destination", dst, "is something weird after deployment."]
        (IsLink _       , _             , _             ) -> error ["The source", src, "is a symbolic link."]
        _                                                 -> error ["The source", src, "is something weird."]

  where
    src = normalise s
    dst = normalise d

    fine :: Sparker (Maybe String)
    fine = return Nothing

    error :: [String] -> Sparker (Maybe String)
    error err = return $ Just $ unwords err

    compareFile :: Sparker (Maybe String)
    compareFile = do
        equal <- compareFiles src dst
        if equal
        then fine
        else error ["The source and destination files are somehow still not equal."]

    compareDir :: Sparker (Maybe String)
    compareDir = do
        equal <- compareDirectories src dst
        if equal
        then fine
        else error ["The source and destination directories are somehow still not equal."]

    compareLink :: Sparker (Maybe String)
    compareLink = do
        point <- liftIO $ readSymbolicLink dst
        if point `filePathEqual` src
        then fine
        else do
            debug $ unwords ["The destination points to", point, "but the src points to", src]
            error ["The destination is a symbolic link, but it doesn't point to the source."]

filePathEqual :: FilePath -> FilePath -> Bool
filePathEqual f g = (normalise f) == (normalise g)

-}
