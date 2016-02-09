module Seed where

import           Compiler.Types
import           Deployer.Types
import           Language.Types
import           Monad
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (takeDirectory, (</>))
import           Types

seedByCompiledCardRef :: DeployerCardReference -> [Deployment] -> Sparker [Deployment]
seedByCompiledCardRef (DeployerCardCompiled fp) = seedByRel fp
seedByCompiledCardRef (DeployerCardUncompiled (CardFileReference fp _)) = seedByRel fp

seedByRel :: FilePath -> [Deployment] -> Sparker [Deployment]
seedByRel file ds = do
    cur <- liftIO $ getCurrentDirectory
    let reldir = takeDirectory file
    return $ seed (cur </> reldir) ds

seed :: FilePath -> [Deployment] -> [Deployment]
seed fp = map (\d -> d { deployment_srcs = map seedsrc $ deployment_srcs d })
  where
    seedsrc :: FilePath -> FilePath
    seedsrc = (fp </>)
