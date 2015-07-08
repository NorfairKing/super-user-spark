module Dispatch where

import           Data.List          (isPrefixOf)
import           System.Environment (getArgs)

import           Types


loadConfig :: [String] -> SparkConfig
loadConfig as = Config {}
  where
    args = filter ("-" `isPrefixOf`) as

    loadFormatOptions :: FormatOptions
    loadFormatOptions =
        if present "--compress"
        then FormatOptions {
                conf_format_lineUp = False
            ,   conf_format_indent = 0
            ,   conf_format_trailingNewline = False
            ,   conf_format_alwaysQuote = False
            }
        else FormatOptions {
                conf_format_lineUp = notPresent "--no-line-up"
            ,   conf_format_indent = "--indent" `withDefault` 4
            ,   conf_format_trailingNewline = notPresent "--no-trailing-newline"
            ,   conf_format_alwaysQuote = present "--always-quote"
            }



    present :: String -> Bool
    present = (`elem` args)

    notPresent :: String -> Bool
    notPresent = not . present

    withDefault :: Read a => String -> a -> a
    withDefault flag def = go args flag def
      where
        go [] _ def = def
        go [_] _ def = def
        go (f:v:fs) flag def | f == flag = read v
                             | otherwise = go (v:fs) flag def


