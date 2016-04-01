module Config where

import           Config.Types

defaultConfig :: SparkConfig
defaultConfig = Config
    { conf_compile_output             = Nothing
    , conf_compile_format             = FormatJson
    , conf_compile_kind               = Nothing
    , conf_compile_override           = Nothing
    , conf_deploy_replace_links       = False
    , conf_deploy_replace_files       = False
    , conf_deploy_replace_directories = False
    , conf_debug                      = False
    }
