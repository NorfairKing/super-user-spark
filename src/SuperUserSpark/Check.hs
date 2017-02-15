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

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Utils

checkFromArgs :: CheckArgs -> IO ()
checkFromArgs cas = do
    errOrAss <- checkAssignment cas
    case errOrAss of
        Left err -> die $ unwords ["Failed to build Check assignment:", err]
        Right ass -> check ass

checkAssignment :: CheckArgs -> IO (Either String CheckAssignment)
checkAssignment CheckArgs {..} = do
    errOrCardRef <- parseBakeCardReference checkArgCardRef
    case errOrCardRef of
        Left err -> pure $ Left err
        Right cardRef ->
            CheckAssignment cardRef <$$> deriveCheckSettings cardRef checkFlags

deriveCheckSettings :: BakeCardReference
                    -> CheckFlags
                    -> IO (Either String CheckSettings)
deriveCheckSettings bcr CheckFlags {..} =
    CheckSettings <$$> deriveBakeSettings bcr checkBakeFlags

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
formatCheckError (CheckBakeError ce) = formatBakeError ce
formatCheckError (CheckError s) = unwords ["Check failed:", s]

checkByCardRef :: BakeCardReference -> SparkChecker ()
checkByCardRef checkCardReference = do
    deps <-
        checkerBake $ compileBakeCardRef checkCardReference >>= bakeDeployments
    dcrs <- liftIO $ checkDeployments deps
    liftIO $ putStrLn $ formatDeploymentChecks $ zip deps dcrs

checkerBake :: SparkBaker a -> SparkChecker a
checkerBake =
    withExceptT CheckBakeError . mapExceptT (withReaderT checkBakeSettings)

checkDeployments :: [BakedDeployment] -> IO [DeploymentCheckResult]
checkDeployments ds = do
    diagnosed <- mapM diagnoseDeployment ds
    return $ map checkDeployment diagnosed
