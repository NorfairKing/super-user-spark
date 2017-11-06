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
import SuperUserSpark.Diagnose
import SuperUserSpark.Diagnose.Types
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
    CheckSettings <$$> deriveDiagnoseSettings bcr checkDiagnoseFlags

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
formatCheckError (CheckDiagnoseError ce) = formatDiagnoseError ce
formatCheckError (CheckError s) = unwords ["Check failed:", s]

checkByCardRef :: BakeCardReference -> SparkChecker ()
checkByCardRef checkCardReference = do
    ddeps <-
        checkerDiagnose $
        diagnoserBake
            (compileBakeCardRef checkCardReference >>= bakeDeployments) >>=
        (liftIO . diagnoseDeployments)
    liftIO $ putStrLn $ formatDeploymentChecks $ zip ddeps $ checkDeployments ddeps

checkerDiagnose :: SparkDiagnoser a -> SparkChecker a
checkerDiagnose =
    withExceptT CheckDiagnoseError .
    mapExceptT (withReaderT checkDiagnoseSettings)

checkDeployments :: [DiagnosedDeployment] -> [DeploymentCheckResult]
checkDeployments = map checkDeployment
