module Check.Types where

import           CoreTypes
import           Data.Digest.Pure.MD5 (MD5Digest)

type HashDigest = MD5Digest

data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLinkTo FilePath
    | IsWeird
    deriving (Show, Eq)

data DiagnosedFp = D
    { diagnosedFilePath    :: FilePath
    , diagnosedDiagnostics :: Diagnostics
    , diagnosedHashDigest  :: HashDigest
    } deriving (Show, Eq)


data DeploymentCheckResult
    = DeploymentDone
    | ReadyToDeploy FilePath FilePath DeploymentKind
    | DirtySituation String
    | Impossible String
    deriving (Show, Eq)

data CheckResult = AlreadyDone
                 | Ready
                 | Dirty String
    deriving (Show, Eq)

data DiagnosedDeployment = Diagnosed
    { diagnosedSrcs :: [DiagnosedFp]
    , diagnosedDst  :: DiagnosedFp
    , diagnosedKind :: DeploymentKind
    } deriving (Show, Eq)

