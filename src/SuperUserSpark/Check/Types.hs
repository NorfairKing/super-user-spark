{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Check.Types where

import Import

import Data.Hashable

import SuperUserSpark.Bake.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Compiler.Types

data CheckAssignment = CheckAssignment
    { checkCardReference :: BakeCardReference
    , checkSettings :: CheckSettings
    } deriving (Show, Eq, Generic)

instance Validity CheckAssignment

data CheckSettings = CheckSettings
    { checkBakeSettings :: BakeSettings
    } deriving (Show, Eq, Generic)

instance Validity CheckSettings

defaultCheckSettings :: CheckSettings
defaultCheckSettings = CheckSettings {checkBakeSettings = defaultBakeSettings}

type SparkChecker = ExceptT CheckError (ReaderT CheckSettings IO)

data CheckError
    = CheckBakeError BakeError
    | CheckError String
    deriving (Show, Eq, Generic)

instance Validity CheckError

newtype HashDigest =
    HashDigest Int
    deriving (Show, Eq, Generic)

instance Validity HashDigest

instance Monoid HashDigest where
    mempty = HashDigest (hash ())
    (HashDigest h1) `mappend` (HashDigest h2) = HashDigest $ h1 * 31 + h2

instance Hashable HashDigest

data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLinkTo AbsP -- Could point to directory too.
    | IsWeird
    deriving (Show, Eq, Generic)

instance Validity Diagnostics

data DiagnosedFp = D
    { diagnosedFilePath :: AbsP
    , diagnosedDiagnostics :: Diagnostics
    , diagnosedHashDigest :: HashDigest
    } deriving (Show, Eq, Generic)

instance Validity DiagnosedFp where
    isValid D {..} =
        and [isValid diagnosedFilePath, isValid diagnosedDiagnostics]

data Instruction =
    Instruction AbsP
                AbsP
                DeploymentKind
    deriving (Show, Eq, Generic)

instance Validity Instruction

data CleanupInstruction
    = CleanFile (Path Abs File)
    | CleanDirectory (Path Abs Dir)
    | CleanLink (Path Abs File)
    deriving (Show, Eq, Generic)

instance Validity CleanupInstruction

data DeploymentCheckResult
    = DeploymentDone -- ^ Done already
    | ReadyToDeploy Instruction -- ^ Immediately possible
    | DirtySituation String
                     Instruction
                     CleanupInstruction -- ^ Possible after cleanup of destination
    | ImpossibleDeployment [String] -- ^ Entirely impossible
    deriving (Show, Eq, Generic)

instance Validity DeploymentCheckResult

data CheckResult
    = AlreadyDone -- ^ Done already
    | Ready Instruction -- ^ Immediately possible
    | Dirty String
            Instruction
            CleanupInstruction -- ^ Possible after cleanup
    | Impossible String -- ^ Entirely impossible
    deriving (Show, Eq, Generic)

instance Validity CheckResult

data DiagnosedDeployment = Diagnosed
    { diagnosedDirections :: DeploymentDirections DiagnosedFp
    , diagnosedKind :: DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity DiagnosedDeployment
