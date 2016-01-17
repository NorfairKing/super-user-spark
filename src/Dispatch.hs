module Dispatch where

import           Compiler
import           Deployer
import           Formatter
import           Parser
import           Parser.Types
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = parseFile fp >> return () -- Just parse, throw away the results.
dispatch (DispatchFormat fp) = do
    cards <- parseFile fp
    str <- formatCards cards
    liftIO $ putStrLn str
dispatch (DispatchCompile (CardFileReference fp mcnr)) = do
    cards <- parseFile fp
    deployments <- compileRef cards mcnr
    outputCompiled deployments
dispatch (DispatchCheck ccr) = do
    deps <- case ccr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled (CardFileReference fp mcnr) -> do
            cards <- parseFile fp
            compileRef cards mcnr
    pdps <- check deps
    liftIO $ putStr $ formatPreDeployments $ zip deps pdps
dispatch (DispatchDeploy dcr) = do
    deps <- case dcr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled scr -> do
            cards <- parseCardFileReference scr
            compile (head cards) cards -- filtering is already done at parse
    deploy deps
