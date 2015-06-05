module Main where

import           System.Directory   (createDirectoryIfMissing,
                                     getCurrentDirectory, getHomeDirectory)
import           System.Environment (getArgs)

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
     (file:[]) -> do
        ecs <- parseFile file
        case ecs of
            Left err -> print err
            Right cs -> do
                    print cs
                    let dp = compile (head cs) cs cdir home
                    print dp
                    deploy dp
     _ -> putStrLn "error parsing arguments"


checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir

