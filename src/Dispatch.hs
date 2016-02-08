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
    deps <- deployCompiledCardRef ccr
    seeded <- seedByCompiledCardRef ccr deps
    pdps <- check seeded
    liftIO $ putStr $ formatPreDeployments $ zip seeded pdps

dispatch (DispatchDeploy dcr) = do
    deps <- deployCompiledCardRef dcr
    seeded <- seedByCompiledCardRef dcr deps
    deploy seeded


deployCompiledCardRef :: DeployerCardReference -> Sparker [Deployment]
deployCompiledCardRef (DeployerCardCompiled fp) = inputCompiled fp
deployCompiledCardRef (DeployerCardUncompiled cfr) = compileJob cfr
