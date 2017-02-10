module Check
    ( check
    , formatDeploymentChecks
    ) where

import Import

import Check.Internal
import Check.Types
import Compiler.Types
import Deployer.Internal

check :: [Deployment] -> IO [DeploymentCheckResult]
check ds = do
    completed <- completeDeployments ds
    diagnosed <- mapM diagnoseDeployment completed
    return $ map checkDeployment diagnosed
