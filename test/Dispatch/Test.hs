{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Dispatch.Test (htf_thisModulesTests) where

import           Test.Framework
import           Test.HUnit                    (Assertion)
import           Text.ParserCombinators.Parsec

import           Dispatch
import           Types

testFlag :: (SparkConfig -> Bool) -> String -> Assertion
testFlag f fs = assertBool $ f conf
  where conf = loadConfig (words fs)

testOption :: (Show a, Eq a) => (SparkConfig -> a) -> a -> String -> Assertion
testOption f d fs = assertEqual (f conf) d
  where conf = loadConfig (words fs)

testMultiple :: String -> SparkConfig -> Assertion
testMultiple fs exp = assertEqual exp conf
  where conf = loadConfig (words fs)

test_loadConfig_format_lineUp_true   = testFlag (conf_format_lineUp . conf_format)                "                       "
test_loadConfig_format_lineUp_false  = testFlag (not . conf_format_lineUp . conf_format)          " --no-line-up          "
test_loadConfig_format_indent_0      = testOption (conf_format_indent . conf_format)   0          " --indent 0            "
test_loadConfig_format_indent_4      = testOption (conf_format_indent . conf_format)   4          " --indent 4            "
test_loadConfig_format_indent_12     = testOption (conf_format_indent . conf_format)  12          " --indent 12           "
test_loadConfig_format_newline_true  = testFlag (conf_format_trailingNewline . conf_format)       "                       "
test_loadConfig_format_newline_false = testFlag (not . conf_format_trailingNewline . conf_format) " --no-trailing-newline "
test_loadConfig_format_quote_true    = testFlag (conf_format_alwaysQuote . conf_format)           " --always-quote        "
test_loadConfig_format_quote_false   = testFlag (not . conf_format_alwaysQuote . conf_format)     "                       "
test_loadConfig_format_compress      = testMultiple "--compress" $
    defaultConfig {
        conf_format = FormatOptions {
                conf_format_lineUp          = False
            ,   conf_format_indent          = 0
            ,   conf_format_trailingNewline = False
            ,   conf_format_alwaysQuote     = False
            }
        }


