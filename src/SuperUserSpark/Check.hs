module SuperUserSpark.Check
    ( check
    , formatDeploymentChecks
    ) where

import Import

import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Internal

check :: [Deployment] -> IO [DeploymentCheckResult]
check ds = do
    completed <- completeDeployments ds
    diagnosed <- mapM diagnoseDeployment completed
    return $ map checkDeployment diagnosed
