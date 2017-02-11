{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Check
    ( checkFromArgs
    , checkAssignment
    , deriveCheckSettings
    , check
    , formatCheckError
    , formatDeploymentChecks
    , checkDeployments
    ) where

import Import

import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Seed
import SuperUserSpark.Utils

checkFromArgs :: CheckArgs -> IO ()
checkFromArgs cas = do
    errOrAss <- checkAssignment cas
    case errOrAss of
        Left err -> die $ unwords ["Failed to build Check assignment:", err]
        Right ass -> check ass

checkAssignment :: CheckArgs -> IO (Either String CheckAssignment)
checkAssignment CheckArgs {..} =
    CheckAssignment <$$> pure (readEither checkArgCardRef) <**>
    deriveCheckSettings checkFlags

deriveCheckSettings :: CheckFlags -> IO (Either String CheckSettings)
deriveCheckSettings CheckFlags {..} =
    CheckSettings <$$> deriveCompileSettings checkCompileFlags

check :: CheckAssignment -> IO ()
check CheckAssignment {..} = do
    errOrDone <-
        runReaderT
            (runExceptT $ checkByCardRef checkCardReference)
            checkSettings
    case errOrDone of
        Left err -> die $ formatCheckError err
        Right () -> pure ()

formatCheckError :: CheckError -> String
formatCheckError (CheckCompileError ce) = formatCompileError ce
formatCheckError (CheckError s) = unwords ["Check failed:", s]

checkByCardRef :: CheckCardReference -> SparkChecker ()
checkByCardRef checkCardReference = do
    deps <- compileCheckCardRef checkCardReference
    seeded <- liftIO $ seedByCheckCardRef checkCardReference deps
    dcrs <- liftIO $ checkDeployments seeded
    liftIO $ putStrLn $ formatDeploymentChecks $ zip seeded dcrs

compileCheckCardRef :: CheckCardReference -> SparkChecker [Deployment]
compileCheckCardRef (CheckCardCompiled fp) = checkerCompile $ inputCompiled fp
compileCheckCardRef (CheckCardUncompiled cfr) = checkerCompile $ compileJob cfr

seedByCheckCardRef :: CheckCardReference -> [Deployment] -> IO [Deployment]
seedByCheckCardRef (CheckCardCompiled fp) = seedByRel fp
seedByCheckCardRef (CheckCardUncompiled (CardFileReference fp _)) = seedByRel fp

checkerCompile :: ImpureCompiler a -> SparkChecker a
checkerCompile =
    withExceptT CheckCompileError .
    mapExceptT (withReaderT checkCompileSettings)

checkDeployments :: [Deployment] -> IO [DeploymentCheckResult]
checkDeployments ds = do
    completed <- completeDeployments ds
    diagnosed <- mapM diagnoseDeployment completed
    return $ map checkDeployment diagnosed
