module Dispatch (dispatch) where

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

dispatch (DispatchCheck ccr) = do
    deps <- compileDeployerCardRef ccr
    seeded <- seedByCompiledCardRef ccr deps
    pdps <- check seeded
    liftIO $ putStr $ formatPreDeployments $ zip seeded pdps

dispatch (DispatchDeploy dcr) = do
    deps <- compileDeployerCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    deploy seeded


compileDeployerCardRef :: DeployerCardReference -> Sparker [Deployment]
compileDeployerCardRef (DeployerCardCompiled fp) = inputCompiled fp
compileDeployerCardRef (DeployerCardUncompiled cfr) = compileJob cfr
