{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Deployer where

import Import

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Diagnose
import SuperUserSpark.Diagnose.Types
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
            DeployAssignment cardRef <$$>
            deriveDeploySettings cardRef deployFlags

deriveDeploySettings :: BakeCardReference
                     -> DeployFlags
                     -> IO (Either String DeploySettings)
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
    deployAbss deps

deployerBake :: SparkBaker a -> SparkDeployer a
deployerBake =
    withExceptT (DeployCheckError . CheckDiagnoseError . DiagnoseBakeError) .
    mapExceptT
        (withReaderT $
         diagnoseBakeSettings . checkDiagnoseSettings . deployCheckSettings)

deployAbss :: [BakedDeployment] -> SparkDeployer ()
deployAbss ds = do
    stage1
    stage2
    stage3
  where
    stage1 = do
        ddeps <- liftIO $ diagnoseDeployments ds
        let dcrs = checkDeployments ddeps
        -- Check for impossible deployments
        when (any impossibleDeployment dcrs) $
            err (zip ddeps dcrs) "Deployment is impossible."
        -- Clean up the situation
        forM_ dcrs $ \d -> do
            case d of
                DirtySituation _ _ cis -> performClean cis
                _ -> return ()
    stage2
      -- Check again
     = do
        ddeps2 <- liftIO $ diagnoseDeployments ds
        let dcrs2 = checkDeployments ddeps2
        -- Error if the cleaning is not done now.
        when (any (\d -> impossibleDeployment d || dirtyDeployment d) dcrs2) $
            err (zip ddeps2 dcrs2) $
            unlines
                [ "Situation was not entirely clean after attemted cleanup."
                , "Maybe you forgot to enable cleanups (--replace-all)?"
                ]
        -- Perform deployments
        liftIO $
            mapM_ performDeployment $
            map (\(ReadyToDeploy i) -> i) $ filter deploymentReadyToDeploy dcrs2
    stage3
        -- Check one last time.
     = do
        do ddeps3 <- liftIO $ diagnoseDeployments ds
           let dcrsf3 = flip map ddeps3 $ \ddep -> (ddep, checkDeployment ddep)
            -- If the check result is dirty, but the deployment was a pipe,
            -- then that's to be expected
           let isDone =
                   and $
                   flip map dcrsf3 $ \(ddep, cr) ->
                       case cr of
                           DeploymentDone -> True
                           DirtySituation _ _ _ ->
                               case deploymentKind ddep of
                                   PipeDeployment _ -> True
                                   _ -> False
                           _ -> False
           when (not isDone) $ do
               err
                   dcrsf3
                   "Something went wrong during deployment. It's not done yet."
    err :: [(DiagnosedDeployment, DeploymentCheckResult)]
        -> String
        -> SparkDeployer ()
    err dcrs_ text = do
        liftIO $ putStrLn $ formatDeploymentChecks dcrs_
        throwError $ DeployError text
