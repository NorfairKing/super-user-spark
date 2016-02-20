module Check where

import           Check.Types
import           Types

checkDeployment :: DiagnosedDeployment -> CheckResult
checkDeployment (Diagnosed [] (D dst _ _) _)
    = Dirty $ unwords ["No source for deployment with destination", dst]
checkDeployment _ = Dirty "Not yet implemented"

checkSingle :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> CheckResult
checkSingle (D src srcd srch) (D dst dstd dsth) kind =
    case (srcd, dstd, kind) of
        (Nonexistent, _             , _             )
            -> e ["The source:", src, "does not exist."]

        (IsFile     , Nonexistent   , _             ) -> Ready

        (IsFile     , IsFile        , LinkDeployment)
            -> e ["Both the source:", src, "and the destination:", dst, "are files for a link deployment."]

        (IsFile     , IsFile        , CopyDeployment)
            -> if srch == dsth
                then AlreadyDone
                else e ["Both the source:", src, "and the destination:", dst, "are files for a copy deployment, but they are not equal."]

        (IsFile     , IsDirectory   , _)
            -> e ["The source: ", src, "is a file but the destination:", dst, "is a directory."]

        (IsFile     , IsLinkTo l    , LinkDeployment)
            -> if l == src
                then AlreadyDone
                else e ["The source:", src, "is a file and the destination:", dst, "is a link for a link deployment but the destination does not point to the source:", l, "."]

        (IsFile     , IsLinkTo _    , CopyDeployment)
            -> e ["The source:", src, "is a file and the destination:", dst, "is a link for a copy deployment."]

        (IsDirectory, Nonexistent   , _             ) -> Ready

        (IsDirectory, IsFile        , _             )
            -> e ["The source:", src, "is a directory and the destination:", dst, "is a file."]

        (IsDirectory, IsDirectory   , CopyDeployment)
            -> if srch == dsth
                then AlreadyDone
                else e ["The source:", src, "and destination:", dst, "are directories for a copy deployment, but they are not equal."]

        (IsDirectory, IsDirectory   , LinkDeployment)
            -> e ["The source:", src, "and the destination:", dst, "are directories for a link deployment."]

        (IsDirectory, IsLinkTo l    , LinkDeployment)
            -> if l == src
                then AlreadyDone
                else e ["The source:", src, "is a directory and the destination:", dst, "is a link for a link deployment but the destination does not point to the source:", l, "."]

        (IsDirectory, IsLinkTo _    , CopyDeployment)
            -> e ["The source:", src, "is a directory and the destination:", dst, "is a link for a copy deployment."]

        (IsLinkTo _ , _             , _             )
            -> e ["The source:", src, "is a link."]

        (IsWeird    , IsWeird       , _             )
            -> e ["Both the source:", src, "and the destination:", dst, "are weird."]

        (IsWeird    , _             , _             )
            -> e ["The source:", src, "is weird."]

        (_          , IsWeird       , _             )
            -> e ["The destination:", dst, "is weird."]
  where
    e = Dirty . unwords


