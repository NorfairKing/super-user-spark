module SuperUserSpark.Compiler.TestUtils where

import TestImport

import Data.Either (isLeft, isRight)

import SuperUserSpark.Compiler.Internal
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Types
import SuperUserSpark.PreCompiler
import SuperUserSpark.PreCompiler.Types

runPreCompiler :: Precompiler () -> [PreCompileError]
runPreCompiler pc = runIdentity $ execWriterT pc

cleanBy :: (a -> Precompiler ()) -> a -> Bool
cleanBy func a = null $ runPreCompiler $ func a

declarationClean :: Declaration -> IO ()
declarationClean d = d `shouldSatisfy` cleanBy cleanDeclaration

declarationDirty :: Declaration -> IO ()
declarationDirty d = d `shouldNotSatisfy` cleanBy cleanDeclaration

filePathDirty :: FilePath -> IO ()
filePathDirty fp = fp `shouldNotSatisfy` cleanBy cleanFilePath

filePathClean :: FilePath -> IO ()
filePathClean fp = fp `shouldSatisfy` cleanBy cleanFilePath

runPureCompiler :: CompileSettings -> PureCompiler a -> Either CompileError a
runPureCompiler c func = runIdentity $ runReaderT (runExceptT func) c

runInternalCompiler
    :: [Declaration]
    -> CompilerState
    -> CompileSettings
    -> Either CompileError (CompilerState, ([Deployment], [CardReference]))
runInternalCompiler ds s c =
    runPureCompiler c $ runWriterT $ execStateT (compileDecs ds) s

compileSingleDec
    :: Declaration
    -> CompilerState
    -> CompileSettings
    -> Either CompileError (CompilerState, ([Deployment], [CardReference]))
compileSingleDec d = runInternalCompiler [d]

compilationShouldSucceed :: [Declaration]
                         -> CompilerState
                         -> CompileSettings
                         -> IO ()
compilationShouldSucceed ds s c =
    runInternalCompiler ds s c `shouldSatisfy` isRight

compilationShouldFail :: [Declaration]
                      -> CompilerState
                      -> CompileSettings
                      -> IO ()
compilationShouldFail ds s c = runInternalCompiler ds s c `shouldSatisfy` isLeft

singleShouldFail :: CompileSettings -> CompilerState -> Declaration -> IO ()
singleShouldFail c s d = compilationShouldFail [d] s c

shouldCompileTo :: CompileSettings
                -> CompilerState
                -> [Declaration]
                -> [Deployment]
                -> IO ()
shouldCompileTo c s ds eds = do
    compilationShouldSucceed ds s c
    let Right (_, (ads, crs)) = runInternalCompiler ds s c
    ads `shouldBe` eds
    crs `shouldSatisfy` null

singleShouldCompileTo :: CompileSettings
                      -> CompilerState
                      -> Declaration
                      -> Deployment
                      -> IO ()
singleShouldCompileTo c s d eds = shouldCompileTo c s [d] [eds]

shouldResultInState :: CompileSettings
                    -> CompilerState
                    -> Declaration
                    -> CompilerState
                    -> IO ()
shouldResultInState c s d es = do
    compilationShouldSucceed [d] s c
    let Right (as, _) = runInternalCompiler [d] s c
    as `shouldBe` es

-- Filepath utils
containsNewlineCharacter :: String -> Bool
containsNewlineCharacter f = any (\c -> elem c f) ['\n', '\r']
