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

singleShouldFail
    :: SparkConfig
    -> CompilerState
    -> Declaration
    -> IO ()
singleShouldFail c s d = compilationShouldFail [d] s c

shouldCompileTo
    :: SparkConfig
    -> CompilerState
    -> [Declaration]
    -> [Deployment]
    -> IO ()
shouldCompileTo c s ds eds = do
    compilationShouldSucceed ds s c
    let Right (_, (ads, crs)) = runInternalCompiler ds s c
    ads `shouldBe` eds
    crs `shouldSatisfy` null

singleShouldCompileTo
    :: SparkConfig
    -> CompilerState
    -> Declaration
    -> Deployment
    -> IO ()
singleShouldCompileTo c s d eds = shouldCompileTo c s [d] [eds]

shouldResultInState
    :: SparkConfig
    -> CompilerState
    -> Declaration
    -> CompilerState
    -> IO ()
shouldResultInState c s d es = do
    compilationShouldSucceed [d] s c
    let Right (as, _) = runInternalCompiler [d] s c
    as `shouldBe` es



-- Filepath utils
containsNewlineCharacter f = any (\c -> elem c f) ['\n', '\r']
