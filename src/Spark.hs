module Spark (spark) where

import           Arguments
import           Dispatch
import           Types
import           Utils

spark :: IO ()
spark = do
    (di, config) <- getInstructions

    er <- runSparker config $ dispatch di
    case er of
        Left err -> die $ showError err
        Right _ -> return ()

showError :: SparkError -> String
showError (ParseError err) = show err
showError (PreCompileError errs) = unlines errs
showError (CompileError err) = err
showError (UnpredictedError err) = "Panic: " ++ err
showError (DeployError err) = err
