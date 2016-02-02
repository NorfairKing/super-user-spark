module Compiler.TestUtils where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Either       (isLeft, isRight)

import           Compiler.Internal
import           Compiler.Types
import           Parser.Types
import           Types

runInternalCompiler
     :: [Declaration]
    -> CompilerState
    -> SparkConfig
    -> Either CompileError (CompilerState, ([Deployment], [CardReference]))
runInternalCompiler ds s c = runIdentity $ runReaderT (runExceptT $ runWriterT $ execStateT (compileDecs ds) s) c

compileSingleDec
    :: Declaration
    -> CompilerState
    -> SparkConfig
    -> Either CompileError (CompilerState, ([Deployment], [CardReference]))
compileSingleDec d = runInternalCompiler [d]


compilationShouldSucceed
    :: [Declaration]
    -> CompilerState
    -> SparkConfig
    -> IO ()
compilationShouldSucceed ds s c = runInternalCompiler ds s c `shouldSatisfy` isRight

compilationShouldFail
    :: [Declaration]
    -> CompilerState
    -> SparkConfig
    -> IO ()
compilationShouldFail ds s c = runInternalCompiler ds s c `shouldSatisfy` isLeft
