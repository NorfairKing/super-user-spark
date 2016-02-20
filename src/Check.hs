module Check where

import           Check.Types
import           Language.Types
import           Monad
import           System.Directory   (Permissions (..))
import           System.Exit        (ExitCode (..))
import           System.FilePath    (dropFileName, normalise, (</>))
import           System.Posix.Files (createSymbolicLink, fileExist,
                                     getSymbolicLinkStatus, isBlockDevice,
                                     isCharacterDevice, isDirectory,
                                     isNamedPipe, isRegularFile, isSocket,
                                     isSymbolicLink, readSymbolicLink)
import           System.Process     (system)
import           Types

checkDeployment :: DiagnosedDeployment -> CheckResult
checkDeployment (Diagnosed [] (D dst _ _) _)
    = Dirty $ unwords ["No source for deployment with destination", dst]
checkDeployment dd = Dirty "Not yet implemented"

checkSingle :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> CheckResult
checkSingle (D src srcd srch) (D dst dstd dsth) kind =
    case (srcd, dstd, kind) of

        (IsWeird, IsWeird, _)
            -> e ["Both the source:", src, "and the destination", dst, "are weird"]

        (IsWeird, _, _)
            -> e ["The source:", src, "is weird"]

        (_, IsWeird, _)
            -> e ["The destination:", dst, "is weird"]

        _ -> undefined
  where
    e = Dirty . unwords


