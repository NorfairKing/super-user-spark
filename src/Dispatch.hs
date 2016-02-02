module Dispatch where

import           Compiler
import           Deployer
import           Deployer.Types
import           Dispatch.Types
import           Formatter
import           Parser
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = parseFile fp >> return () -- Just parse, throw away the results.
dispatch (DispatchFormat fp) = do
    sf <- parseFile fp
    str <- formatSparkFile sf
    liftIO $ putStrLn str
dispatch (DispatchCompile cfr) = do
    deployments <- compileJob cfr
    outputCompiled deployments
dispatch (DispatchCheck ccr) = do
    deps <- case ccr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled cfr -> compileJob cfr
    pdps <- check deps
    liftIO $ putStr $ formatPreDeployments $ zip deps pdps
dispatch (DispatchDeploy dcr) = do
    deps <- case dcr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled cfr -> compileJob cfr
    deploy deps
