module Types where

import Import

import CoreTypes

---[ Options ]---
data GlobalOptions = GlobalOptions
    { opt_output :: Maybe FilePath
    , opt_kind :: Maybe DeploymentKind
    , opt_overrride :: Maybe DeploymentKind
    , opt_replace_links :: Bool
    , opt_replace_files :: Bool
    , opt_replace_directories :: Bool
    , opt_replace :: Bool
    , opt_debug :: Bool
    } deriving (Show, Eq)
