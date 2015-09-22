module Main where

import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Data.List          (isPrefixOf)

import           Arguments
import           Compiler
import           Deployer
import           Dispatch
import           Formatter
import           Parser
import           Paths
import           Types
import           Utils

main :: IO ()
main = do
    checkSystemConsistency

    (di, config) <- getInstructions

    er <- runSparker config $ dispatch di
    case er of
        Left err -> die $ showError err
        Right _ -> return ()


spark :: StartingSparkReference -> Sparker ()
spark ssr = do
    cs <- parseStartingCardReference ssr
    fcs <- formatCards cs
    debug fcs
    dp <- compile (head cs) cs
    debug $ formatDeployments dp
    deploy dp

showError :: SparkError -> String
showError (ParseError err) = show err
showError (CompileError err) = err
showError (DeployError err) =
    case err of
        PreDeployError ss -> unlines ss
        DuringDeployError ss -> unlines ss
        PostDeployError ss -> unlines ss

showError (UnpredictedError err) = "Panic: " ++ err
showError (GitError err) = show err


checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

