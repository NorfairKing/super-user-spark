module Dispatch where

import           Text.Parsec

import           Compiler
import           Deployer
import           Formatter
import           Parser
import           Types

dispatch :: Dispatch -> Sparker ()
dispatch (DispatchParse fp) = parseFile fp >> return () -- Just parse, throw away the results.
dispatch (DispatchFormat fp) = do
    cards <- parseFile fp
    str <- formatCards cards
    liftIO $ putStrLn str
dispatch (DispatchCompile (CardFileReference fp mcnr)) = do
    cards <- parseFile fp
    deployments <- compileRef cards mcnr
    outputCompiled deployments
dispatch (DispatchCheck ccr) = do
    deps <- case ccr of
        CheckerCardCompiled fp -> inputCompiled fp
        CheckerCardUncompiled (CardFileReference fp mcnr) -> do
            cards <- parseFile fp
            compileRef cards mcnr
    pdps <- check deps
    liftIO $ putStrLn $ unlines $ map show pdps
dispatch (DispatchDeploy dcr) = do
    deps <- case dcr of
        DeployerCardCompiled fp -> inputCompiled fp
        DeployerCardUncompiled scr -> do
            cards <- parseStartingCardReference scr
            compile (head cards) cards -- filtering is already done at parse
    deploy deps

---[ Loading config ]---

loadDispatcher :: [String] -> Either ParseError Dispatch
loadDispatcher strs = parse parseDispatch "Arguments" (unwords strs)

loadConfig :: [String] -> SparkConfig
loadConfig args = Config {
        conf_format_lineUp              = if present "--compress" then False else notPresent "--no-line-up"
    ,   conf_format_indent              = if present "--compress" then 0     else "--indent" `withDefault` 4
    ,   conf_format_trailingNewline     = if present "--compress" then False else notPresent "--no-trailing-newline"
    ,   conf_format_alwaysQuote         = if present "--compress" then False else present "--always-quote"
    ,   conf_format_oneLine             = present "--compress"
    ,   conf_compile_output             = maybeValue "--output"
    ,   conf_compile_format             = "--format"        `withDefault` FormatText
    ,   conf_compile_kind                = maybeValue "--kind"
    ,   conf_compile_override            = maybeValue "--override"
    ,   conf_check_thoroughness         = "--thoroughness"  `withDefault` ThoroughnessContent
    ,   conf_deploy_replace_links       = present "--replace-links"       || present "--replace"
    ,   conf_deploy_replace_files       = present "--replace-files"       || present "--replace"
    ,   conf_deploy_replace_directories = present "--replace-directories" || present "--replace"
    ,   conf_debug                      = present "--debug"
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
