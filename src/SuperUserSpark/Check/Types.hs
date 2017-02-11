{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SuperUserSpark.Check.Types where

import Import

import Data.Digest.Pure.MD5 (MD5Digest)
import System.FilePath hiding (isValid)

import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types

data CheckAssignment = CheckAssignment
    { checkCardReference :: CheckCardReference
    , checkSettings :: CheckSettings
    } deriving (Show, Eq, Generic)

instance Validity CheckAssignment

data CheckCardReference
    = CheckCardCompiled FilePath
    | CheckCardUncompiled CardFileReference
    deriving (Show, Eq, Generic)

instance Validity CheckCardReference

instance Read CheckCardReference where
    readsPrec _ fp =
        case length (words fp) of
            0 -> []
            1 ->
                if takeExtension fp == ".sus"
                    then [ ( CheckCardUncompiled (CardFileReference fp Nothing)
                           , "")
                         ]
                    else [(CheckCardCompiled fp, "")]
            2 ->
                let [f, c] = words fp
                in [ ( CheckCardUncompiled
                           (CardFileReference f (Just $ CardNameReference c))
                     , "")
                   ]
            _ -> []

data CheckSettings = CheckSettings
    { checkCompileSettings :: CompileSettings
    } deriving (Show, Eq, Generic)

instance Validity CheckSettings

defaultCheckSettings :: CheckSettings
defaultCheckSettings =
    CheckSettings {checkCompileSettings = defaultCompileSettings}

type SparkChecker = ExceptT CheckError (ReaderT CheckSettings IO)

data CheckError
    = CheckCompileError CompileError
    | CheckError String
    deriving (Show, Eq, Generic)

instance Validity CheckError

type HashDigest = MD5Digest

data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLinkTo FilePath
    | IsWeird
    deriving (Show, Eq, Generic)

instance Validity Diagnostics

data DiagnosedFp = D
    { diagnosedFilePath :: FilePath
    , diagnosedDiagnostics :: Diagnostics
    , diagnosedHashDigest :: HashDigest
    } deriving (Show, Eq, Generic)

instance Validity DiagnosedFp where
    isValid D {..} =
        and [isValid diagnosedFilePath, isValid diagnosedDiagnostics]

data Instruction =
    Instruction FilePath
                FilePath
                DeploymentKind
    deriving (Show, Eq, Generic)

instance Validity Instruction

data CleanupInstruction
    = CleanFile FilePath
    | CleanDirectory FilePath
    | CleanLink FilePath
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
    { diagnosedSrcs :: [DiagnosedFp]
    , diagnosedDst :: DiagnosedFp
    , diagnosedKind :: DeploymentKind
    } deriving (Show, Eq, Generic)

instance Validity DiagnosedDeployment
