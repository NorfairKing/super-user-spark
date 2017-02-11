module SuperUserSpark
    ( spark
    ) where

import Import

import SuperUserSpark.OptParse
import SuperUserSpark.Dispatch
import SuperUserSpark.Monad
import SuperUserSpark.Utils

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
