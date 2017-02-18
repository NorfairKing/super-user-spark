module SuperUserSpark.Check.Internal where

import Import

import qualified Data.ByteString as SB
import Data.Hashable
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.Posix.Files
       (fileExist, getSymbolicLinkStatus, isBlockDevice,
        isCharacterDevice, isDirectory, isNamedPipe, isRegularFile,
        isSocket, isSymbolicLink, readSymbolicLink)
import System.Process (readProcess, system)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Constants
import SuperUserSpark.CoreTypes

checkDeployment :: DiagnosedDeployment -> DeploymentCheckResult
checkDeployment (Deployment (Directions [] (D dst _ _)) _) =
    ImpossibleDeployment
        [unwords ["No source for deployment with destination", toPath dst]]
checkDeployment (Deployment (Directions srcs dst) kind) =
    bestResult $ map (\src -> checkSingle src dst kind) srcs

bestResult :: [CheckResult] -> DeploymentCheckResult
bestResult cs
    | all impossible cs = ImpossibleDeployment $ map (\(Impossible s) -> s) cs
    | otherwise
        -- Will not be empty as per line above
     =
        case head $ dropWhile impossible cs of
            AlreadyDone -> DeploymentDone
            Ready i -> ReadyToDeploy i
            Dirty s i c -> DirtySituation s i c
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
        (IsFile, Nonexistent, _) -> ready
        (IsFile, IsFile, LinkDeployment) ->
            e
                [ "Both the source:"
                , toPath src
                , "and the destination:"
                , toPath dst
                , "are files for a link deployment."
                ]
        (IsFile, IsFile, CopyDeployment) ->
            if srch == dsth
                then AlreadyDone
                else e
                         [ "Both the source:"
                         , toPath src
                         , "and the destination:"
                         , toPath dst
                         , "are files for a copy deployment, but they are not equal."
                         ]
        (IsFile, IsDirectory, _) ->
            e
                [ "The source: "
                , toPath src
                , "is a file but the destination:"
                , toPath dst
                , "is a directory."
                ]
        (IsFile, IsLinkTo l, LinkDeployment) ->
            if l == src
                then AlreadyDone
                else e
                         [ "The source:"
                         , toPath src
                         , "is a file and the destination:"
                         , toPath dst
                         , "is a link for a link deployment but the destination does not point to the source. Instead it points to:"
                         , toPath l ++ "."
                         ]
        (IsFile, IsLinkTo _, CopyDeployment) ->
            e
                [ "The source:"
                , toPath src
                , "is a file and the destination:"
                , toPath dst
                , "is a link for a copy deployment."
                ]
        (IsDirectory, Nonexistent, _) -> ready
        (IsDirectory, IsFile, _) ->
            e
                [ "The source:"
                , toPath src
                , "is a directory and the destination:"
                , toPath dst
                , "is a file."
                ]
        (IsDirectory, IsDirectory, CopyDeployment) ->
            if srch == dsth
                then AlreadyDone
                else e
                         [ "The source:"
                         , toPath src
                         , "and destination:"
                         , toPath dst
                         , "are directories for a copy deployment, but they are not equal."
                         ]
        (IsDirectory, IsDirectory, LinkDeployment) ->
            e
                [ "The source:"
                , toPath src
                , "and the destination:"
                , toPath dst
                , "are directories for a link deployment."
                ]
        (IsDirectory, IsLinkTo l, LinkDeployment) ->
            if l == src
                then AlreadyDone
                else e
                         [ "The source:"
                         , toPath src
                         , "is a directory and the destination:"
                         , toPath dst
                         , "is a link for a link deployment but the destination does not point to the source. Instead it points to:"
                         , toPath l ++ "."
                         ]
        (IsDirectory, IsLinkTo _, CopyDeployment) ->
            e
                [ "The source:"
                , toPath src
                , "is a directory and the destination:"
                , toPath dst
                , "is a link for a copy deployment."
                ]
        (Nonexistent, _, _) -> i ["The source:", toPath src, "does not exist."]
        (IsLinkTo _, _, _) -> i ["The source:", toPath src, "is a link."]
        (IsWeird, IsWeird, _) ->
            i
                [ "Both the source:"
                , toPath src
                , "and the destination:"
                , toPath dst
                , "are weird."
                ]
        (IsWeird, _, _) -> i ["The source:", toPath src, "is weird."]
        (_, IsWeird, _) -> i ["The destination:", toPath dst, "is weird."]
  where
    ins = Instruction src dst kind
    ready = Ready ins
    i = Impossible . unlines
    e s =
        case dstd of
            IsFile -> Dirty (unlines s) ins $ CleanFile $ unAbsP dst
            IsLinkTo _ -> Dirty (unlines s) ins $ CleanLink $ unAbsP dst
            IsDirectory ->
                case parseAbsDir $ toPath dst of
                    Left err -> Impossible $ show err -- Should not happen, but just in case.
                    Right dir -> Dirty (unlines s) ins $ CleanDirectory dir
            _ -> Impossible "should not occur"

diagnoseDeployment :: BakedDeployment -> IO DiagnosedDeployment
diagnoseDeployment (Deployment bds kind) = do
    ddirs <- diagnoseDirs bds
    return $ Deployment ddirs kind

diagnoseDirs :: DeploymentDirections AbsP
             -> IO (DeploymentDirections DiagnosedFp)
diagnoseDirs (Directions srcs dst) =
    Directions <$> mapM diagnose srcs <*> diagnose dst

diagnose :: AbsP -> IO DiagnosedFp
diagnose fp = do
    d <- diagnoseFp fp
    hash_ <- hashFilePath fp
    return $ D fp d hash_

diagnoseFp :: AbsP -> IO Diagnostics
diagnoseFp absp = do
    let fp = toPath absp
    ms <- forgivingAbsence $ getSymbolicLinkStatus fp
    case ms of
        Nothing -> pure Nonexistent
        Just s ->
            if isBlockDevice s ||
               isCharacterDevice s || isSocket s || isNamedPipe s
                then return IsWeird
                else do
                    if isSymbolicLink s
                        then do
                            point <- readSymbolicLink fp
                            -- TODO check what happens with relative links.
                            apoint <- AbsP <$> parseAbsFile point
                            return $ IsLinkTo apoint
                        else if isDirectory s
                                 then return IsDirectory
                                 else if isRegularFile s
                                          then return IsFile
                                          else error $
                                               "File " ++
                                               fp ++
                                               " was neither a block device, a character device, a socket, a named pipe, a symbolic link, a directory or a regular file"

-- | Hash a filepath so that two filepaths with the same contents have the same hash
hashFilePath :: AbsP -> IO HashDigest
hashFilePath fp = do
    d <- diagnoseFp fp
    case d of
        IsFile -> hashFile fp
        IsDirectory -> hashDirectory fp
        IsLinkTo _ -> return $ HashDigest $ hash ()
        IsWeird -> return $ HashDigest $ hash ()
        Nonexistent -> return $ HashDigest $ hash ()

hashFile :: AbsP -> IO HashDigest
hashFile fp = (HashDigest . hash) <$> SB.readFile (toPath fp)

hashDirectory :: AbsP -> IO HashDigest
hashDirectory fp = do
    tdir <- parseAbsDir (toPath fp)
    walkDirAccum Nothing writer_ tdir
  where
    writer_ _ _ files = do
        hashes <- mapM (hashFile . AbsP) files
        pure $ HashDigest $ hash hashes

formatDeploymentChecks :: [(BakedDeployment, DeploymentCheckResult)] -> String
formatDeploymentChecks dss =
    if null output
        then "Deployment is done already."
        else unlines output ++
             if all (impossibleDeployment . snd) dss
                 then "Deployment is impossible."
                 else "Deployment is possible."
  where
    output = catMaybes $ map formatDeploymentCheck dss

formatDeploymentCheck :: (BakedDeployment, DeploymentCheckResult)
                      -> Maybe String
formatDeploymentCheck (_, (ReadyToDeploy is)) =
    Just $ "READY: " ++ formatInstruction is
formatDeploymentCheck (_, DeploymentDone) = Nothing
formatDeploymentCheck (d, ImpossibleDeployment ds) =
    Just $
    concat
        [ "IMPOSSIBLE: "
        , toPath $ directionDestination $ deploymentDirections d
        , " cannot be deployed:\n"
        , unlines ds
        , "\n"
        ]
formatDeploymentCheck (d, (DirtySituation str is c)) =
    Just $
    concat
        [ "DIRTY: "
        , toPath $ directionDestination $ deploymentDirections d
        , "\n"
        , str
        , "planned: "
        , formatInstruction is
        , "\n"
        , "cleanup needed:\n"
        , formatCleanupInstruction c
        , "\n"
        ]

formatInstruction :: Instruction -> String
formatInstruction (Instruction src dst k) =
    unwords $ [toPath src, kindSymbol k, toPath dst]
  where
    kindSymbol LinkDeployment = linkKindSymbol
    kindSymbol CopyDeployment = copyKindSymbol

formatCleanupInstruction :: CleanupInstruction -> String
formatCleanupInstruction (CleanFile fp) = "remove file " ++ toFilePath fp
formatCleanupInstruction (CleanDirectory dir) =
    "remove directory " ++ toFilePath dir
formatCleanupInstruction (CleanLink link) = "remove link " ++ toFilePath link
