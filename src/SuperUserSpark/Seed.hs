module SuperUserSpark.Seed where

import Import hiding ((</>))

import SuperUserSpark.Compiler.Types
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

seedByRel :: FilePath -> [Deployment] -> IO [Deployment]
seedByRel file ds = do
    cur <- liftIO getCurrentDirectory
    let reldir = takeDirectory file
    return $ seed (cur </> reldir) ds

seed :: FilePath -> [Deployment] -> [Deployment]
seed fp = map (\d -> d {deploymentSources = map seedsrc $ deploymentSources d})
  where
    seedsrc :: FilePath -> FilePath
    seedsrc = (fp </>)
