module Dispatch where

import           Compiler
import           Deployer
import           Deployer.Types
import           Dispatch.Types
import           Formatter
import           Parser
import           Parser.Types
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = parseFile fp >> return () -- Just parse, throw away the results.
dispatch (DispatchFormat fp) = do
    sf <- parseFile fp
    str <- formatSparkFile sf
    liftIO $ putStrLn str
dispatch (DispatchCompile (CardFileReference fp mcnr)) = do
    sf <- parseFile fp
    let cus = resolveUnits sf
    deployments <- compileRef cus mcnr
    outputCompiled deployments
dispatch (DispatchCheck ccr) = do
    deps <- case ccr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled (CardFileReference fp mcnr) -> do
            sf <- parseFile fp
            let cus = resolveUnits sf
            compileRef cus mcnr
    pdps <- check deps
    liftIO $ putStr $ formatPreDeployments $ zip deps pdps
dispatch (DispatchDeploy dcr) = do
    deps <- case dcr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled (CardFileReference fp mcnr) -> do
            sf <- parseFile fp
            let cus = resolveUnits sf
            compileRef cus mcnr
            -- compile (head cards) cards -- filtering is already done at parse
    deploy deps
