{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Deployer where

import Import

import SuperUserSpark.Check
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Seed
import SuperUserSpark.Utils

deployFromArgs :: DeployArgs -> IO ()
deployFromArgs das = do
    case deployAssignment das of
        Left err -> die $ unwords ["Failed to make Deployment assignment:", err]
        Right ass -> deploy ass

deployAssignment :: DeployArgs -> Either String DeployAssignment
deployAssignment DeployArgs {..} =
    DeployAssignment <$> readEither deployArgCardRef <*> deriveDeploySettings deployFlags

deriveDeploySettings :: DeployFlags -> Either String DeploySettings
deriveDeploySettings DeployFlags{..} = do
       cs <- deriveCheckSettings deployCheckFlags
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

deployByCardRef :: DeployerCardReference -> SparkDeployer ()
deployByCardRef dcr = do
    deps <- compileDeployerCardRef dcr
    seeded <- liftIO $ seedByCompiledCardRef dcr deps
    dcrs <- liftIO $ checkDeployments seeded
    deploySeeded $ zip seeded dcrs

compileDeployerCardRef :: DeployerCardReference -> SparkDeployer [Deployment]
compileDeployerCardRef (DeployerCardCompiled fp) =
    deployerCompile $ inputCompiled fp
compileDeployerCardRef (DeployerCardUncompiled cfr) =
    deployerCompile $ compileJob cfr

seedByCompiledCardRef :: DeployerCardReference
                      -> [Deployment]
                      -> IO [Deployment]
seedByCompiledCardRef (DeployerCardCompiled fp) = seedByRel fp
seedByCompiledCardRef (DeployerCardUncompiled (CardFileReference fp _)) =
    seedByRel fp

deployerCompile :: ImpureCompiler a -> SparkDeployer a
deployerCompile =
    withExceptT (DeployCheckError . CheckCompileError) .
    mapExceptT (withReaderT $ checkCompileSettings . deployCheckSettings)

deploySeeded :: [(Deployment, DeploymentCheckResult)] -> SparkDeployer ()
deploySeeded dcrs = do
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
    when (any (impossibleDeployment ||| dirtyDeployment) dcrs2) $
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
    err :: [(Deployment, DeploymentCheckResult)] -> String -> SparkDeployer ()
    err dcrs_ text = do
        liftIO $ putStrLn $ formatDeploymentChecks dcrs_
        throwError $ DeployError text
