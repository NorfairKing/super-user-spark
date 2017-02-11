module SuperUserSpark.Deployer where

import Import

import Control.Monad.Except

import SuperUserSpark.Check
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Monad
import SuperUserSpark.Seed
import SuperUserSpark.Utils

deploy :: DeployerCardReference -> Sparker ()
deploy dcr = do
    deps <- compileDeployerCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    dcrs <- liftIO $ check seeded
    deploySeeded $ zip seeded dcrs

compileDeployerCardRef :: DeployerCardReference -> Sparker [Deployment]
compileDeployerCardRef (DeployerCardCompiled fp) = inputCompiled fp
compileDeployerCardRef (DeployerCardUncompiled cfr) = compileJob cfr

deploySeeded :: [(Deployment, DeploymentCheckResult)] -> Sparker ()
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
    dcrs2 <- liftIO $ check ds
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
    dcrsf <- liftIO $ check ds
    when (any (not . deploymentIsDone) dcrsf) $ do
        err
            (zip ds dcrsf)
            "Something went wrong during deployment. It's not done yet."
  where
    err :: [(Deployment, DeploymentCheckResult)] -> String -> Sparker ()
    err dcrs_ text = do
        liftIO $ putStrLn $ formatDeploymentChecks dcrs_
        throwError $ DeployError text
