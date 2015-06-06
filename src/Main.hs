module Main where

import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Compiler
import           Deployer
import           Parser
import           Paths
import           Types

main :: IO ()
main = do
    checkSystemConsistency
    args <- getArgs
    case args of
     (file:_) -> do
        config <- loadConfig
        runSparker config $ do
            ecs <- parseFile file
            case ecs of
                Left err -> liftIO $ print err
                Right cs -> do
                        liftIO $ putStrLn $ formatCards cs
                        dp <- compile (head cs) cs
                        liftIO $ putStrLn $ formatDeployments dp
                        deploy dp
     _ -> putStrLn "error parsing arguments"


loadConfig :: IO SparkConfig
loadConfig = do
    (file:flags) <- getArgs
    return $ Config {
        conf_dry = True -- FTM "--dry" `elem` flags
    }

checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

