module Deployer where

import           System.Directory   (copyFile)
import           System.Posix.Files (createSymbolicLink)

import           Types

deploy :: [Deployment] -> IO ()
deploy dp = sequence_ $ map oneDeployment dp

oneDeployment :: Deployment -> IO ()
oneDeployment (Link src dst) = createSymbolicLink src dst
oneDeployment (Copy src dst) = copyFile src dst
