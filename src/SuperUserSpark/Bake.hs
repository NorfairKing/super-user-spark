{-# LANGUAGE RecordWildCards #-}

{-
    The responsibility of the baker is to turn raw deployments into baked
    deployments. This takes care of everything that couldn't happen during
    compilation yet. The differences between raw deployments and baked
    deployments are:
    - Baked deployments only deal with absolute filepaths so as to be
      working-directory-independent.
    - Baked deployments are aware of the kind of things the checker/deployer
      will be operating on (files versus directories).

    The baker is not responsible for checking any existences.
-}
module SuperUserSpark.Bake where

import Import

import qualified Data.Aeson.Encode.Pretty as JSON
import System.Environment (getEnvironment)
import System.FilePath (takeExtension)

import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Types
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
    BakeAssignment <$$> parseBakeCardReference bakeCardRef <**>
    deriveBakeSettings bakeFlags

parseBakeCardReference :: String -> IO (Either String BakeCardReference)
parseBakeCardReference s =
    case words s of
        [fp] ->
            if takeExtension fp == ".sus"
                then BakeCardUncompiled <$$> parseStrongCardFileReference fp
                else BakeCardCompiled <$$> resolveFile'Either fp
        [f, c] ->
            BakeCardUncompiled <$$>
            ((\(StrongCardFileReference p _) ->
                  StrongCardFileReference p (Just $ CardNameReference c)) <$$>
             parseStrongCardFileReference f)
        _ -> pure $ Left $ unwords ["Could not parse card reference from:", s]

deriveBakeSettings :: BakeFlags -> IO (Either String BakeSettings)
deriveBakeSettings BakeFlags {..} =
    BakeSettings <$$> (Right <$> getEnvironment) <**>
    deriveCompileSettings bakeCompileFlags

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
    bdeps <- bakeDeployments deps
    putStrLn $ JSON.encodePretty bdeps

compileBakeCardRef :: BakeCardReference -> SparkBaker [Deployment]
compileBakeCardRef (BakeCardCompiled fp) = bakerCompile $ inputCompiled fp
compileBakeCardRef (BakeCardUncompiled bcf) = bakerCompile $ compileJob bcf

bakerCompile :: ImpureCompiler a -> SparkBaker a
bakerCompile =
    withExceptT BakeCompileError . mapExceptT (withReaderT bakeCompileSettings)
