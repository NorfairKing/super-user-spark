module Dispatch where

import           Types

data Dispatch = DispatchParse FilePath
              | DispatchFormat FilePath
              | DispatchCompile FilePath (Maybe CardName)
              | DispatchCheck FilePath (Maybe CardName)
              | DispatchDeploy StartingSparkReference
