module SuperUserSpark.Dispatch
    ( dispatch
    ) where

import Import

import SuperUserSpark.Check
import SuperUserSpark.Compiler
import SuperUserSpark.Deployer
import SuperUserSpark.Dispatch.Types
import SuperUserSpark.Monad
import SuperUserSpark.Parser
import SuperUserSpark.Seed

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = do
    void $ parseFile fp -- Just parse, throw away the results.
dispatch (DispatchCompile cfr) = do
    deployments <- compileJob cfr
    outputCompiled deployments
dispatch (DispatchCheck dcr) = do
    deps <- compileDeployerCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    dcrs <- liftIO $ check seeded
    liftIO $ putStrLn $ formatDeploymentChecks $ zip seeded dcrs
dispatch (DispatchDeploy dcr) = deploy dcr
