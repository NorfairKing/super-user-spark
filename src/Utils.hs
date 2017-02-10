{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

import Import

import Control.Monad.Reader

import Config.Types
import Control.Monad (when)
import Data.List (isInfixOf)
import qualified System.Directory as D (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types

debug
    :: (MonadReader SparkConfig m, MonadIO m)
    => String -> m ()
debug str = incase (asks conf_debug) $ liftIO $ putStrLn str

incase
    :: MonadReader SparkConfig m
    => (SparkConfig -> Bool) -> m () -> m ()
incase bf func = do
    b <- asks bf
    when b func

incaseElse
    :: MonadReader SparkConfig m
    => (SparkConfig -> Bool) -> m a -> m a -> m a
incaseElse bf funcif funcelse = do
    b <- asks bf
    if b
        then funcif
        else funcelse

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

containsNewline :: String -> Bool
containsNewline f = any (\c -> elem c f) ['\n', '\r']

containsMultipleConsequtiveSlashes :: String -> Bool
containsMultipleConsequtiveSlashes = isInfixOf "//"

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g = \a -> f a && g a

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g = \a -> f a || g a

createDirectoryIfMissing :: FilePath -> IO ()
createDirectoryIfMissing = D.createDirectoryIfMissing True
