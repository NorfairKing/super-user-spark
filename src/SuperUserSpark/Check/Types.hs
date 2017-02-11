{-# LANGUAGE DeriveGeneric #-}

module SuperUserSpark.Check.Types where

import Import

import Data.Digest.Pure.MD5 (MD5Digest)
import System.FilePath

import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types

data CheckAssignment = CheckAssignment
    { checkCardReference :: CheckCardReference
    , checkSettings :: CheckSettings
    } deriving (Show, Eq, Generic)

data CheckCardReference
    = CheckCardCompiled FilePath
    | CheckCardUncompiled CardFileReference
    deriving (Show, Eq, Generic)

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

defaultCheckSettings :: CheckSettings
defaultCheckSettings =
    CheckSettings {checkCompileSettings = defaultCompileSettings}

type SparkChecker = ExceptT CheckError (ReaderT CheckSettings IO)

data CheckError
    = CheckCompileError CompileError
    | CheckError String
    deriving (Show, Eq, Generic)

type HashDigest = MD5Digest

data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLinkTo FilePath
    | IsWeird
    deriving (Show, Eq)

data DiagnosedFp = D
    { diagnosedFilePath :: FilePath
    , diagnosedDiagnostics :: Diagnostics
    , diagnosedHashDigest :: HashDigest
    } deriving (Show, Eq)

data Instruction =
    Instruction FilePath
                FilePath
                DeploymentKind
    deriving (Show, Eq)

data CleanupInstruction
    = CleanFile FilePath
    | CleanDirectory FilePath
    | CleanLink FilePath
    deriving (Show, Eq)

data DeploymentCheckResult
    = DeploymentDone -- ^ Done already
    | ReadyToDeploy Instruction -- ^ Immediately possible
    | DirtySituation String
                     Instruction
                     CleanupInstruction -- ^ Possible after cleanup of destination
    | ImpossibleDeployment [String] -- ^ Entirely impossible
    deriving (Show, Eq)

data CheckResult
    = AlreadyDone -- ^ Done already
    | Ready Instruction -- ^ Immediately possible
    | Dirty String
            Instruction
            CleanupInstruction -- ^ Possible after cleanup
    | Impossible String -- ^ Entirely impossible
    deriving (Show, Eq)

data DiagnosedDeployment = Diagnosed
    { diagnosedSrcs :: [DiagnosedFp]
    , diagnosedDst :: DiagnosedFp
    , diagnosedKind :: DeploymentKind
    } deriving (Show, Eq)
