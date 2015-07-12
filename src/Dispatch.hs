module Dispatch where

import           Text.Parsec

import           Parser
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch dp = do
    return ()

loadDispatcher :: [String] -> Either ParseError Dispatch
loadDispatcher strs = parse parseDispatch "Arguments" (unwords strs)


loadConfig :: [String] -> SparkConfig
loadConfig args = Config {
        conf_format_lineUp              = if present "--compress" then False else notPresent "--no-line-up"
    ,   conf_format_indent              = if present "--compress" then 0     else "--indent" `withDefault` 4
    ,   conf_format_trailingNewline     = if present "--compress" then False else notPresent "--no-trailing-newline"
    ,   conf_format_alwaysQuote         = if present "--compress" then False else present "--always-quote"
    ,   conf_compile_output             = maybeValue "--output"
    ,   conf_compile_format             = "--format"        `withDefault` FormatText
    ,   conf_check_thoroughness         = "--thoroughness"  `withDefault` ThoroughnessContent
    ,   conf_debug                      = present "--debug"
    ,   conf_deploy_kind                = "--kind"          `withDefault` LinkDeployment
    ,   conf_deploy_override            = maybeValue "--override"
    ,   conf_deploy_replace_links       = present "--replace-links"       || present "--replace"
    ,   conf_deploy_replace_files       = present "--replace-files"       || present "--replace"
    ,   conf_deploy_replace_directories = present "--replace-directories" || present "--replace"
    }
  where

    present :: String -> Bool
    present = (`elem` args)

    notPresent :: String -> Bool
    notPresent = not . present

    maybeValue :: Read a => String -> Maybe a
    maybeValue flag = go args
      where
        go [] = Nothing
        go [_] = Nothing
        go (f:v:fs) | f == flag = read v
                    | otherwise = go (v:fs)

    withDefault :: Read a => String -> a -> a
    withDefault flag def = go args
      where
        go [] = def
        go [_] = def
        go (f:v:fs) | f == flag = read v
                    | otherwise = go (v:fs)


defaultConfig :: SparkConfig
defaultConfig = loadConfig []
