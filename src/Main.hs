module Main where

import           System.Environment (getArgs)

import           Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
     (file:[]) -> do
        cs <- parseFile file
        print cs
     _ -> putStrLn "error parsing arguments"
