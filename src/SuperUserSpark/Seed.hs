module SuperUserSpark.Seed where

import Import

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Types
import SuperUserSpark.Language.Types
import SuperUserSpark.Monad
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

seedByCompiledCardRef :: DeployerCardReference
                      -> [Deployment]
                      -> Sparker [Deployment]
seedByCompiledCardRef (DeployerCardCompiled fp) = seedByRel fp
seedByCompiledCardRef (DeployerCardUncompiled (CardFileReference fp _)) =
    seedByRel fp

seedByRel :: FilePath -> [Deployment] -> Sparker [Deployment]
seedByRel file ds = do
    cur <- liftIO $ getCurrentDirectory
    let reldir = takeDirectory file
    return $ seed (cur </> reldir) ds

seed :: FilePath -> [Deployment] -> [Deployment]
seed fp = map (\d -> d {deploymentSources = map seedsrc $ deploymentSources d})
  where
    seedsrc :: FilePath -> FilePath
    seedsrc = (fp </>)
