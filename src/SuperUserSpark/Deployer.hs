{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Deployer where

import Import

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Deployer.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Utils

deployFromArgs :: DeployArgs -> IO ()
deployFromArgs das = do
    errOrAss <- deployAssignment das
    case errOrAss of
        Left err -> die $ unwords ["Failed to make Deployment assignment:", err]
        Right ass -> deploy ass

deployAssignment :: DeployArgs -> IO (Either String DeployAssignment)
deployAssignment DeployArgs {..} = do
    errOrCardRef <- parseBakeCardReference deployArgCardRef
    case errOrCardRef of
        Left err -> pure $ Left err
        Right cardRef ->
            DeployAssignment cardRef <$$> deriveDeploySettings cardRef deployFlags

deriveDeploySettings :: BakeCardReference -> DeployFlags -> IO (Either String DeploySettings)
deriveDeploySettings bcr DeployFlags {..} = do
    ecs <- deriveCheckSettings bcr deployCheckFlags
    pure $ do
        cs <- ecs
        pure
            DeploySettings
            { deploySetsReplaceLinks =
                  deployFlagReplaceLinks || deployFlagReplaceAll
            , deploySetsReplaceFiles =
                  deployFlagReplaceFiles || deployFlagReplaceAll
            , deploySetsReplaceDirectories =
                  deployFlagReplaceDirectories || deployFlagReplaceAll
            , deployCheckSettings = cs
            }

deploy :: DeployAssignment -> IO ()
deploy DeployAssignment {..} = do
    errOrDone <-
        runReaderT
            (runExceptT $ deployByCardRef deployCardReference)
            deploySettings
    case errOrDone of
        Left err -> die $ formatDeployError err
        Right () -> pure ()

formatDeployError :: DeployError -> String
formatDeployError (DeployCheckError e) = formatCheckError e
formatDeployError (DeployError s) = unwords ["Deployment failed:", s]

deployByCardRef :: BakeCardReference -> SparkDeployer ()
deployByCardRef dcr = do
    deps <- deployerBake $ compileBakeCardRef dcr >>= bakeDeployments
    dcrs <- liftIO $ checkDeployments deps
    deployAbss $ zip deps dcrs

deployerBake :: SparkBaker a -> SparkDeployer a
deployerBake =
    withExceptT (DeployCheckError . CheckBakeError) .
    mapExceptT (withReaderT $ checkBakeSettings . deployCheckSettings)

deployAbss :: [(BakedDeployment, DeploymentCheckResult)] -> SparkDeployer ()
deployAbss dcrs = do
    let crs = map snd dcrs
    -- Check for impossible deployments
    when (any impossibleDeployment crs) $ err dcrs "Deployment is impossible."
    -- Clean up the situation
    forM_ crs $ \d -> do
        case d of
            DirtySituation _ _ cis -> performClean cis
            _ -> return ()
    -- Check again
    let ds = map fst dcrs
    dcrs2 <- liftIO $ checkDeployments ds
    -- Error if the cleaning is not done now.
    when (any (\d -> impossibleDeployment d || dirtyDeployment d) dcrs2) $
        err
            (zip ds dcrs2)
            "Situation was not entirely clean after attemted cleanup. Maybe you forgot to enable cleanups (--replace-all)?"
    -- Perform deployments
    liftIO $
        mapM_ performDeployment $
        map (\(ReadyToDeploy i) -> i) $ filter deploymentReadyToDeploy dcrs2
    -- Check one last time.
    dcrsf <- liftIO $ checkDeployments ds
    when (any (not . deploymentIsDone) dcrsf) $ do
        err
            (zip ds dcrsf)
            "Something went wrong during deployment. It's not done yet."
  where
    err :: [(BakedDeployment, DeploymentCheckResult)]
        -> String
        -> SparkDeployer ()
    err dcrs_ text = do
        liftIO $ putStrLn $ formatDeploymentChecks dcrs_
        throwError $ DeployError text
