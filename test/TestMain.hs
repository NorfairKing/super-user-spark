{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Parser.Test
import {-@ HTF_TESTS @-} Compiler.Test
import {-@ HTF_TESTS @-} Dispatch.Test
import                   Test.Framework
import                   Test.Framework.BlackBoxTest

main = htfMain htf_importedTests
