module Config.Types where

import           CoreTypes

data SparkConfig = Config
    { conf_compile_output             :: Maybe FilePath
    , conf_compile_format             :: CompileFormat
    , conf_compile_kind               :: Maybe DeploymentKind
    , conf_compile_override           :: Maybe DeploymentKind
    , conf_deploy_replace_links       :: Bool
    , conf_deploy_replace_files       :: Bool
    , conf_deploy_replace_directories :: Bool
    , conf_debug                      :: Bool
    } deriving (Show, Eq)

data CompileFormat = FormatJson
    deriving (Show, Eq)

instance Read CompileFormat where
    readsPrec _ "json"       = [(FormatJson,"")]
    readsPrec _ _ = []

