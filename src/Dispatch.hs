module Dispatch (dispatch) where

import           Check
import           Compiler
import           Compiler.Types
import           Control.Monad  (void)
import           Deployer
import           Deployer.Types
import           Dispatch.Types
import           Formatter
import           Parser
import           Seed
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = do
    void $ parseFile fp  -- Just parse, throw away the results.

dispatch (DispatchFormat fp) = do
    sf <- parseFile fp
    str <- formatSparkFile sf
    liftIO $ putStrLn str

dispatch (DispatchCompile cfr) = do
    deployments <- compileJob cfr
    outputCompiled deployments

dispatch (DispatchCheck dcr) = do
    deps <- compileDeployerCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    dcrs <- liftIO $ check seeded
    liftIO $ putStrLn $ formatDeploymentChecks $ zip seeded dcrs

dispatch (DispatchDeploy dcr) = do
    deps <- compileDeployerCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    dcrs <- liftIO $ check seeded
    pdcs <- deploy dcrs
    liftIO $ putStrLn $ formatDeploymentChecks $ zip seeded pdcs

compileDeployerCardRef :: DeployerCardReference -> Sparker [Deployment]
compileDeployerCardRef (DeployerCardCompiled fp) = inputCompiled fp
compileDeployerCardRef (DeployerCardUncompiled cfr) = compileJob cfr
