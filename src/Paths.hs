module Paths where

import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath                ((<.>), (</>))

import           Types

sparkDir :: IO FilePath
sparkDir = getUserDataDir "spark"

repoDir :: Repo -> IO FilePath
repoDir repo = do
    spark <- sparkDir
    return $ spark </> repo_path repo
