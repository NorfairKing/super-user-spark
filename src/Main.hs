module Main where

import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Parser
import           Paths
import           Types

main :: IO ()
main = do
    args <- getArgs
    case args of
     (file:[]) -> do
        cs <- parseFile file
        print cs
     _ -> putStrLn "error parsing arguments"


checkSystemConsistency :: IO ()
checkSystemConsistency = do
    dir <- sparkDir
    createDirectoryIfMissing True dir


sparkGitRepo :: Repo -> IO ()
sparkGitRepo repo = do
    print =<< repoDir repo


cloneRepo :: Repo -> IO ()
cloneRepo = do
    return ()
