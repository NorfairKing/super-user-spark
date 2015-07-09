module Dispatch where

import           Data.List          (isPrefixOf)
import           System.Environment (getArgs)
import           Text.Parsec
import           Text.Parsec.String

import           Parser
import           Types

dispatch :: [String] -> Maybe Dispatch
dispatch strs = case parse parser "Arguments" (unwords strs) of
                    Left pe -> Nothing
                    Right d -> Just d
  where
    parser = try parseParse
         <|> try parseFormat
         <|> try parseCompile
         <|> try parseCheck
         <|> try parseDeploy

parseParse :: Parser Dispatch
parseParse = do
    string "parse"
    fp <- filepath
    return $ DispatchParse fp

parseFormat :: Parser Dispatch
parseFormat = do
    string "format"
    fp <- filepath
    return $ DispatchFormat fp

parseCompile :: Parser Dispatch
parseCompile = do
    string "compile"
    scr <- startingCardReference
    return $ DispatchCompile $ scr

parseCheck :: Parser Dispatch
parseCheck = do
    string "check"
    cfr <- cardFileReference
    return $ DispatchCheck $ CheckerCardUncompiled cfr

parseDeploy :: Parser Dispatch
parseDeploy = do
    string "deploy"
    scr <- startingCardReference
    return $ DispatchDeploy $ DeployerCardUncompiled scr



config :: [String] -> SparkConfig
config args = Config {
        conf_format  = loadFormatOptions
    ,   conf_compile = loadCompileOptions
    ,   conf_check   = loadCheckOptions
    ,   conf_deploy  = loadDeployOptions
    }
  where
    loadFormatOptions :: FormatOptions
    loadFormatOptions =
        if present "--compress"
        then FormatOptions {
                conf_format_lineUp          = False
            ,   conf_format_indent          = 0
            ,   conf_format_trailingNewline = False
            ,   conf_format_alwaysQuote     = False
            }
        else FormatOptions {
                conf_format_lineUp          = notPresent "--no-line-up"
            ,   conf_format_indent          = "--indent" `withDefault` 4
            ,   conf_format_trailingNewline = notPresent "--no-trailing-newline"
            ,   conf_format_alwaysQuote     = present "--always-quote"
            }

    loadCompileOptions :: CompileOptions
    loadCompileOptions = CompileOptions {
            conf_compile_output = maybeValue "--output"
        ,   conf_compile_format = "--format" `withDefault` FormatText
        }

    loadCheckOptions :: CheckOptions
    loadCheckOptions = CheckOptions {
            conf_check_thoroughness = "--thoroughness" `withDefault` ThoroughnessContent
        }

    loadDeployOptions :: DeployOptions
    loadDeployOptions = DeployOptions {
            conf_deploy_kind         = "--kind" `withDefault` LinkDeployment
        ,   conf_deploy_override     = maybeValue "--override"
        ,   conf_replace_links       = present "--replace-links"       || present "--replace"
        ,   conf_replace_files       = present "--replace-files"       || present "--replace"
        ,   conf_replace_directories = present "--replace-directories" || present "--replace"
        }



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


