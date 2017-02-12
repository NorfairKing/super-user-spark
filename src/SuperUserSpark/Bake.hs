{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Bake where

import Import

import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Utils

bakeFromArgs :: BakeArgs -> IO ()
bakeFromArgs ba = do
    errOrAss <- bakeAssignment ba
    case errOrAss of
        Left be -> die $ unwords ["Failed to build bake assignment:", be]
        Right ass -> bake ass

bakeAssignment :: BakeArgs -> IO (Either String BakeAssignment)
bakeAssignment BakeArgs {..} =
    BakeAssignment <$$> pure (readEither bakeCardRef) <**>
    deriveBakeSettings bakeFlags

deriveBakeSettings :: BakeFlags -> IO (Either String BakeSettings)
deriveBakeSettings BakeFlags {..} =
    BakeSettings <$$> deriveCompileSettings bakeCompileFlags

bake :: BakeAssignment -> IO ()
bake BakeAssignment {..} = do
    errOrDone <-
        runReaderT (runExceptT $ bakeByCardRef bakeCardReference) bakeSettings
    case errOrDone of
        Left err -> die $ formatBakeError err
        Right () -> pure ()

formatBakeError :: BakeError -> String
formatBakeError (BakeCompileError ce) = formatCompileError ce
formatBakeError (BakeError s) = unwords ["Bake failed:", s]

bakeByCardRef :: BakeCardReference -> SparkBaker ()
bakeByCardRef bakeCardReference = do
    deps <- compileBakeCardRef bakeCardReference
    liftIO $ print deps

compileBakeCardRef :: BakeCardReference -> SparkBaker [Deployment]
compileBakeCardRef (BakeCardCompiled fp) = bakerCompile $ inputCompiled fp
compileBakeCardRef (BakeCardUncompiled bcf) = bakerCompile $ compileJob bcf

bakerCompile :: ImpureCompiler a -> SparkBaker a
bakerCompile =
    withExceptT BakeCompileError . mapExceptT (withReaderT bakeCompileSettings)
