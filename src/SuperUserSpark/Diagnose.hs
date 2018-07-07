{-# LANGUAGE RecordWildCards #-}

{-
    The responsibility of the diagnoser is to turn baked deployments into
    diagnosed deployments. This is purely a read-only operation that looks
    at the current state of the file system.
-}
module SuperUserSpark.Diagnose
    ( diagnoseFromArgs
    , diagnoseAssignment
    , deriveDiagnoseSettings
    , diagnose
    , formatDiagnoseError
    , diagnoserBake
    , diagnoseDeployments
    ) where

import Import

import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Diagnose.Internal
import SuperUserSpark.Diagnose.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Utils

diagnoseFromArgs :: DiagnoseArgs -> IO ()
diagnoseFromArgs cas = do
    errOrAss <- diagnoseAssignment cas
    case errOrAss of
        Left err -> die $ unwords ["Failed to build Diagnose assignment:", err]
        Right ass -> diagnose ass

diagnoseAssignment :: DiagnoseArgs -> IO (Either String DiagnoseAssignment)
diagnoseAssignment DiagnoseArgs {..} = do
    errOrCardRef <- parseBakeCardReference diagnoseArgCardRef
    case errOrCardRef of
        Left err -> pure $ Left err
        Right cardRef ->
            DiagnoseAssignment cardRef <$$>
            deriveDiagnoseSettings cardRef diagnoseFlags

deriveDiagnoseSettings :: BakeCardReference
                       -> DiagnoseFlags
                       -> IO (Either String DiagnoseSettings)
deriveDiagnoseSettings bcr DiagnoseFlags {..} =
    DiagnoseSettings <$$> deriveBakeSettings bcr diagnoseBakeFlags

diagnose :: DiagnoseAssignment -> IO ()
diagnose DiagnoseAssignment {..} = do
    errOrDone <-
        runReaderT
            (runExceptT $ diagnoseByCardRef diagnoseCardReference)
            diagnoseSettings
    case errOrDone of
        Left err -> die $ formatDiagnoseError err
        Right () -> pure ()

formatDiagnoseError :: DiagnoseError -> String
formatDiagnoseError (DiagnoseBakeError ce) = formatBakeError ce
formatDiagnoseError (DiagnoseError s) = unwords ["Diagnose failed:", s]

diagnoseByCardRef :: BakeCardReference -> SparkDiagnoser ()
diagnoseByCardRef checkCardReference = do
    deps <-
        diagnoserBake $
        compileBakeCardRef checkCardReference >>= bakeDeployments
    ddeps <- liftIO $ diagnoseDeployments deps
    liftIO . LB.putStrLn $ JSON.encodePretty ddeps

diagnoserBake :: SparkBaker a -> SparkDiagnoser a
diagnoserBake =
    withExceptT DiagnoseBakeError .
    mapExceptT (withReaderT diagnoseBakeSettings)

diagnoseDeployments :: [BakedDeployment] -> IO [DiagnosedDeployment]
diagnoseDeployments = mapM diagnoseDeployment