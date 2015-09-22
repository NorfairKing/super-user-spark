module Main where

import           System.Exit (die)

import           Arguments
import           Dispatch
import           Types

main :: IO ()
main = do
    (di, config) <- getInstructions

    er <- runSparker config $ dispatch di
    case er of
        Left err -> die $ showError err
        Right _ -> return ()

showError :: SparkError -> String
showError (ParseError err) = show err
showError (CompileError err) = err
showError (UnpredictedError err) = "Panic: " ++ err
showError (DeployError err) =
    case err of
        PreDeployError ss -> unlines ss
        DuringDeployError ss -> unlines ss
        PostDeployError ss -> unlines ss

