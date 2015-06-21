module Main where

import           Data.List          (isPrefixOf)
import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Compiler
import           Deployer
import           Formatter
import           Parser
import           Paths
import           Types
import           Utils

main :: IO ()
main = do
    checkSystemConsistency

    config <- loadConfig
    er <- runSparker config $ do
        verbose $ show config

        scr <- loadStartingCardReference
        verbose $ show scr
        spark scr
    case er of
        Left err -> putStrLn $ showError err
        _ -> return ()

loadConfig :: IO SparkConfig
loadConfig = do
    as <- getArgs
    let args = filter ("-" `isPrefixOf`) as
    return $ Config {
            conf_dry = not $ "--no-dry" `elem` args
        ,   conf_verbose = "--verbose" `elem` args || "-v" `elem` args
        }

loadStartingCardReference :: Sparker StartingSparkReference
loadStartingCardReference = do
    as <- liftIO getArgs
    let args = filter (\a -> not $ "-" `isPrefixOf`a) as
    let escr = parseCardReference $ unwords args
    case escr of
        Left pe -> error $ show pe -- TODO better error handling here
        Right scr -> return scr

spark :: StartingSparkReference -> Sparker ()
spark ssr = do
    cs <- parseStartingCardReference ssr
    fcs <- formatCards cs
    verbose fcs
    dp <- compile (head cs) cs
    verboseOrDry $ formatDeployments dp
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

