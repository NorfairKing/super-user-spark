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

checkFromArgs :: CheckArgs -> IO ()
checkFromArgs = check . checkAssignment

checkAssignment :: CheckArgs -> CheckAssignment
checkAssignment CheckArgs {..} =
    CheckAssignment
    { checkCardReference = read checkArgCardRef -- TODO handle failures
    , checkSettings = deriveCheckSettings checkFlags
    }

deriveCheckSettings :: CheckFlags -> CheckSettings
deriveCheckSettings CheckFlags {..} =
    CheckSettings
    {checkCompileSettings = deriveCompileSettings checkCompileFlags}

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
