module Check.Internal where

import           Check.Types
import           Compiler.Types
import           Constants
import           CoreTypes
import qualified Data.ByteString       as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy  as LB
import qualified Data.Digest.Pure.MD5  as H (md5)
import           Data.Maybe            (catMaybes)
import           System.Directory      (getDirectoryContents)
import           System.Exit           (ExitCode (..))
import           System.FilePath       ((</>))
import           System.Posix.Files    (fileExist, getSymbolicLinkStatus,
                                        isBlockDevice, isCharacterDevice,
                                        isDirectory, isNamedPipe, isRegularFile,
                                        isSocket, isSymbolicLink,
                                        readSymbolicLink)
import           System.Process        (readProcess, system)

checkDeployment :: DiagnosedDeployment -> DeploymentCheckResult
checkDeployment (Diagnosed [] (D dst _ _) _)
    = ImpossibleDeployment [unwords ["No source for deployment with destination", dst]]
checkDeployment (Diagnosed srcs dst kind)
    = bestResult $ map (\src -> checkSingle src dst kind) srcs

bestResult :: [CheckResult] -> DeploymentCheckResult
bestResult cs
    | all impossible cs = ImpossibleDeployment $ map (\(Impossible s) -> s) cs
    | otherwise
        -- Will not be empty as per line above
        = case head $ dropWhile impossible cs of
            AlreadyDone  -> DeploymentDone
            Ready i      -> ReadyToDeploy i
            Dirty s i c  -> DirtySituation s i c
            Impossible _ -> error "Cannot be the case"

impossible :: CheckResult -> Bool
impossible (Impossible _) = True
impossible _ = False

impossibleDeployment :: DeploymentCheckResult -> Bool
impossibleDeployment (ImpossibleDeployment _) = True
impossibleDeployment _ = False

dirtyDeployment :: DeploymentCheckResult -> Bool
dirtyDeployment (DirtySituation _ _ _) = True
dirtyDeployment _ = False

deploymentReadyToDeploy :: DeploymentCheckResult -> Bool
deploymentReadyToDeploy (ReadyToDeploy _) = True
deploymentReadyToDeploy _ = False

deploymentIsDone :: DeploymentCheckResult -> Bool
deploymentIsDone DeploymentDone = True
deploymentIsDone _ = False


-- | Check a single (@source@, @destination@, @kind@) triple.
checkSingle :: DiagnosedFp -> DiagnosedFp -> DeploymentKind -> CheckResult
checkSingle (D src srcd srch) (D dst dstd dsth) kind =
    case (srcd, dstd, kind) of
        (IsFile     , Nonexistent   , _             ) -> ready

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
                else e ["The source:", src, "is a file and the destination:", dst, "is a link for a link deployment but the destination does not point to the source. Instead it points to:", l ++ "."]

        (IsFile     , IsLinkTo _    , CopyDeployment)
            -> e ["The source:", src, "is a file and the destination:", dst, "is a link for a copy deployment."]

        (IsDirectory, Nonexistent   , _             ) -> ready

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
                else e ["The source:", src, "is a directory and the destination:", dst, "is a link for a link deployment but the destination does not point to the source. Instead it points to:", l ++ "."]

        (IsDirectory, IsLinkTo _    , CopyDeployment)
            -> e ["The source:", src, "is a directory and the destination:", dst, "is a link for a copy deployment."]

        (Nonexistent, _             , _             )
            -> i ["The source:", src, "does not exist."]

        (IsLinkTo _ , _             , _             )
            -> i ["The source:", src, "is a link."]

        (IsWeird    , IsWeird       , _             )
            -> i ["Both the source:", src, "and the destination:", dst, "are weird."]

        (IsWeird    , _             , _             )
            -> i ["The source:", src, "is weird."]

        (_          , IsWeird       , _             )
            -> i ["The destination:", dst, "is weird."]
  where
    ins = Instruction src dst kind
    ready = Ready ins
    i = Impossible . unlines
    e s = Dirty (unlines s) ins cins
    cins = case dstd of
            IsFile -> CleanFile dst
            IsLinkTo _ -> CleanLink dst
            IsDirectory -> CleanDirectory dst
            _ -> error "should not occur"

diagnoseDeployment :: Deployment -> IO DiagnosedDeployment
diagnoseDeployment (Put srcs dst kind) = do
    dsrcs <- mapM diagnose srcs
    ddst <- diagnose dst
    return $ Diagnosed dsrcs ddst kind

diagnose :: FilePath -> IO DiagnosedFp
diagnose fp = do
    d <- diagnoseFp fp
    hash <- hashFilePath fp
    return $ D fp d hash

diagnoseFp :: FilePath -> IO Diagnostics
diagnoseFp fp = do
    e <- fileExist fp
    if e
    then do
        s <- getSymbolicLinkStatus fp
        if isBlockDevice s || isCharacterDevice s || isSocket s || isNamedPipe s
        then return IsWeird
        else do
            if isSymbolicLink s
            then do
                point <- readSymbolicLink fp
                return $ IsLinkTo point
            else if isDirectory s
                then return IsDirectory
                else if isRegularFile s
                    then return IsFile
                    else error $ "File " ++ fp ++ " was neither a block device, a character device, a socket, a named pipe, a symbolic link, a directory or a regular file"
    else do
        -- If a link exists, but it points to something that doesn't exist, it is considered as non-existent by `fileExist`
        es <- system $ unwords ["test", "-L", fp]
        case es of
            ExitSuccess -> do
                -- Need to do a manual call because readSymbolicLink fails for nonexistent destinations
                point <- readProcess "readlink" [fp] ""
                return $ IsLinkTo $ init point -- remove newline
            ExitFailure _ -> return Nonexistent


md5 :: SB.ByteString -> HashDigest
md5 bs = H.md5 $ LB.fromStrict bs

-- | Hash a filepath so that two filepaths with the same contents have the same hash
hashFilePath :: FilePath -> IO HashDigest
hashFilePath fp = do
    d <- diagnoseFp fp
    case d of
        IsFile -> hashFile fp
        IsDirectory -> hashDirectory fp
        IsLinkTo _ -> return $ md5 SB.empty
        IsWeird -> return $ md5 SB.empty
        Nonexistent -> return $ md5 SB.empty

hashFile :: FilePath -> IO HashDigest
hashFile fp = md5 <$> SB.readFile fp

hashDirectory :: FilePath -> IO HashDigest
hashDirectory fp = do
    rawContents <- getDirectoryContents fp
    let contents = map (fp </>) . filter (\f -> not $ f == "." || f == "..") $ rawContents
    hashes <- mapM hashFilePath contents
    let hashbs = map (SBC.pack . show) hashes
    return $ md5 $ SB.concat hashbs


formatDeploymentChecks :: [(Deployment, DeploymentCheckResult)] -> String
formatDeploymentChecks dss
    = if null output
        then "Deployment is done already."
        else unlines output ++
            if all (impossibleDeployment . snd) dss
             then "Deployment is impossible."
             else "Deployment is possible."
    where output = catMaybes $ map formatDeploymentCheck dss


formatDeploymentCheck :: (Deployment, DeploymentCheckResult) -> Maybe String
formatDeploymentCheck (_, (ReadyToDeploy is))         = Just $ "READY: " ++ formatInstruction is
formatDeploymentCheck (_, DeploymentDone)             = Nothing
formatDeploymentCheck (d, ImpossibleDeployment ds)
    = Just $ "IMPOSSIBLE: "
    ++ deployment_dst d
    ++ " cannot be deployed:\n"
    ++ unlines ds ++ "\n"
formatDeploymentCheck (d, (DirtySituation str is c))
    = Just $ "DIRTY: "
    ++ deployment_dst d
    ++ "\n"
    ++ str
    ++ "planned: "
    ++ formatInstruction is
    ++ "\n"
    ++ "cleanup needed:\n"
    ++ formatCleanupInstruction c
    ++ "\n"

formatInstruction :: Instruction -> String
formatInstruction (Instruction src dst k) = unwords $
    [ src
    , kindSymbol k
    , dst
    ]
  where
    kindSymbol LinkDeployment = linkKindSymbol
    kindSymbol CopyDeployment = copyKindSymbol

formatCleanupInstruction :: CleanupInstruction -> String
formatCleanupInstruction (CleanFile fp)         = "remove file " ++ fp
formatCleanupInstruction (CleanDirectory dir)   = "remove directory " ++ dir
formatCleanupInstruction (CleanLink link)       = "remove link " ++ link


