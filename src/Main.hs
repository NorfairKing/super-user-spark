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
            cs <- parseFile file
            css <- formatCards cs
            liftIO $ putStrLn css
            dp <- compile (head cs) cs
            liftIO $ putStrLn $ formatDeployments dp
            deploy dp
        case er of
            Left err -> putStrLn $ showError err
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


showError :: SparkError -> String
showError (ParseError err) = show err
showError (CompileError err) = err
showError (DeployError err) = show err
showError (UnpredictedError err) = show $ "Panic: " ++ err
