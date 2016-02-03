{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import           Data.List   (isInfixOf)
import           System.Exit (exitFailure)
import           System.IO   (hPutStrLn, stderr)
import           Types

debug :: (MonadReader SparkConfig m, MonadIO m) => String -> m ()
debug str = do
    v <- asks conf_debug
    if v
    then liftIO $ putStrLn str
    else return ()

incase :: MonadReader SparkConfig m => (SparkConfig -> Bool) -> m () -> m ()
incase bf func = do
    b <- asks bf
    if b
    then func
    else return ()

incaseElse :: MonadReader SparkConfig m => (SparkConfig -> Bool) -> m a -> m a -> m a
incaseElse bf funcif funcelse = do
    b <- asks bf
    if b
    then funcif
    else funcelse

notImplementedYet :: Sparker ()
notImplementedYet = throwError $ UnpredictedError "This feature is not implemented yet, it will be in the future, so be sure to check back in a newer version."

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

containsNewline :: String -> Bool
containsNewline f = any (\c -> elem c f) ['\n', '\r']

containsMultipleConsequtiveSlashes :: String -> Bool
containsMultipleConsequtiveSlashes = isInfixOf "//"

