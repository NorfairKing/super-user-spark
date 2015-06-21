module Utils where

import           Types

verbose :: String -> Sparker ()
verbose str = do
    v <- asks conf_verbose
    if v
    then liftIO $ putStrLn str
    else return ()

verboseOrDry :: String -> Sparker ()
verboseOrDry str = do
    v <- asks conf_verbose
    d <- asks conf_dry
    if v || d
    then liftIO $ putStrLn str
    else return ()

