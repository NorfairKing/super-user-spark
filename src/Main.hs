module Main where

import           Control.Monad.IO.Class (liftIO)
import           System.Directory       (createDirectoryIfMissing,
                                         getCurrentDirectory, getHomeDirectory)
import           System.Environment     (getArgs)

import           Compiler
import           Deployer
import           Parser
import           Paths
import           Types

main :: IO ()
main = do
    checkSystemConsistency
    cdir <- getCurrentDirectory
    home <- getHomeDirectory
    args <- getArgs
    case args of
     (file:flags) -> runSparker Config $ do
        ecs <- parseFile file
        case ecs of
            Left err -> liftIO $ print err
            Right cs -> do
                    liftIO $ putStrLn $ formatCards cs
                    dp <- compile (head cs) cs cdir home
                    liftIO $ putStrLn $ formatDeployments dp
                    if ("--dry" `elem` flags)
                    then return ()
                    else deploy dp
     _ -> putStrLn "error parsing arguments"

checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

