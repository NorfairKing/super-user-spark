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
            scr <- loadStartingCardReference
            liftIO $ print scr
            spark scr
        case er of
            Left err -> putStrLn $ showError err
            _ -> return ()
     _ -> putStrLn "error parsing arguments"

loadConfig :: IO SparkConfig
loadConfig = do
    args <- getArgs
    return $ Config {
            conf_dry = not $ "--no-dry" `elem` args
        }

loadStartingCardReference :: Sparker StartingSparkReference
loadStartingCardReference = do
    args <- liftIO getArgs
    let escr = parseCardReference $ unwords args
    case escr of
        Left pe -> error $ show pe -- TODO better error handling here
        Right scr -> return scr

spark :: StartingSparkReference -> Sparker ()
spark ssr = do
    cs <- parseStartingCardReference ssr
    css <- formatCards cs
    liftIO $ putStrLn css
    dp <- compile (head cs) cs
    liftIO $ putStrLn $ formatDeployments dp
    deploy dp

showError :: SparkError -> String
showError (ParseError err) = show err
showError (CompileError err) = err
showError (DeployError err) = show err
showError (UnpredictedError err) = "Panic: " ++ err
showError (GitError err) = show err


checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

