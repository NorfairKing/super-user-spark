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
     (file:flags) -> runSparker Config $ do
        ecs <- parseFile file
        case ecs of
            Left err -> liftIO $ print err
            Right cs -> do
                    liftIO $ putStrLn $ formatCards cs
                    dp <- compile (head cs) cs
                    liftIO $ putStrLn $ formatDeployments dp
                    if ("--dry" `elem` flags)
                    then return ()
                    else deploy dp
     _ -> putStrLn "error parsing arguments"


loadConfig :: IO SparkConfig
loadConfig = return Config

checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

