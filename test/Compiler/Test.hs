{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Compiler.Test (htf_thisModulesTests) where

import           Test.Framework
import           Test.HUnit     (Assertion)

import           Compiler
import           Types

{-
compilerTest :: [Declaration] -> [Deployment] -> Assertion
compilerTest dc dp = assertEqual dp $ compile dc

test_compiler_empty = compilerTest [] []

test_compiler_single_deploy = compilerTest [Deploy "bashrc" ".bashrc" LinkDeployment] [Link "bashrc" ".bashrc"]
--test_compiler_single_spark  = compilerTest
test_compiler_single_into   = compilerTest [IntoDir "~/.xmonad"] []
test_compiler_single_outof  = compilerTest [OutofDir "bash"] []

test_compiler_into          = compilerTest
    [IntoDir "~"
    ,Deploy "bashrc" ".bashrc" LinkDeployment
    ,IntoDir "~/.xmonad"
    ,Deploy "xmonad.hs" "xmonad.hs" LinkDeployment]
    [Link "bashrc" "~/.bashrc", Link "xmonad.hs" "~/.xmonad/xmonad.hs"]

test_compiler_outof         = compilerTest
    [OutofDir "bash"
    ,Deploy "bashrc" "~/.bashrc" LinkDeployment]
    [Link "bash/bashrc" "~/.bashrc"]

test_compiler_both          = compilerTest
    [IntoDir "~/.xmonad"
    ,OutofDir "xmonad"
    ,Deploy "xmonad.hs" "xmonad.hs" LinkDeployment]
    [Link "xmonad/xmonad.hs" "~/.xmonad/xmonad.hs"]
-}

