module Seed where

import           Compiler.Types
import           Monad
import           System.FilePath ((</>))

seed :: FilePath -> [Deployment] -> Sparker [Deployment]
seed fp ds = return $ map (\d -> d { deployment_srcs = map seedsrc $ deployment_srcs d }) ds
  where
    seedsrc :: FilePath -> FilePath
    seedsrc = (fp </>)
