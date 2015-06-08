module Main where

import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Compiler
import           Deployer
import           Formatter
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
        er <- runSparker config $ do
            ecs <- parseFile file
            case ecs of
                Left err -> liftIO $ print err
                Right cs -> do
                        css <- formatCards cs
                        liftIO $ putStrLn css
                        dp <- compile (head cs) cs
                        liftIO $ putStrLn $ formatDeployments dp
                        deploy dp
        case er of
            Left err -> error err
            _ -> return ()
     _ -> putStrLn "error parsing arguments"


loadConfig :: IO SparkConfig
loadConfig = do
    (_:flags) <- getArgs
    return $ Config {
        conf_dry = not $ "--no-dry" `elem` flags
    }

checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

