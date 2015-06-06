module Deployer where

import           System.Directory   (copyFile)
import           System.Posix.Files (createSymbolicLink)

import           Types

deploy :: [Deployment] -> Sparker ()
deploy dp = sequence_ $ map oneDeployment dp

oneDeployment :: Deployment -> Sparker ()
oneDeployment (Link src dst) = liftIO $ createSymbolicLink src dst
oneDeployment (Copy src dst) = liftIO $ copyFile src dst

formatDeployments :: [Deployment] -> String
formatDeployments = unlines . map formatDeployment

formatDeployment :: Deployment -> String
formatDeployment (Link src dst) = unwords [src, "l->", dst]
formatDeployment (Copy src dst) = unwords [src, "c->", dst]

