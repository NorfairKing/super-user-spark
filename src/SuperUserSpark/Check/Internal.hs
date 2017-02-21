module SuperUserSpark.Check.Internal where

import Import

import Data.Maybe (catMaybes)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Constants
import SuperUserSpark.CoreTypes
import SuperUserSpark.Diagnose.Types

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

formatDeploymentChecks :: [(DiagnosedDeployment, DeploymentCheckResult)]
                       -> String
formatDeploymentChecks dss =
    if null output
        then "Deployment is done already."
        else unlines output ++
             if all (impossibleDeployment . snd) dss
                 then "Deployment is impossible."
                 else "Deployment is possible."
  where
    output = catMaybes $ map formatDeploymentCheck dss

formatDeploymentCheck :: (DiagnosedDeployment, DeploymentCheckResult)
                      -> Maybe String
formatDeploymentCheck (_, (ReadyToDeploy is)) =
    Just $ "READY: " ++ formatInstruction is
formatDeploymentCheck (_, DeploymentDone) = Nothing
formatDeploymentCheck (d, ImpossibleDeployment ds) =
    Just $
    concat
        [ "IMPOSSIBLE: "
        , toPath $
          diagnosedFilePath $ directionDestination $ deploymentDirections d
        , " cannot be deployed:\n"
        , unlines ds
        , "\n"
        ]
formatDeploymentCheck (d, (DirtySituation str is c)) =
    Just $
    concat
        [ "DIRTY: "
        , toPath $
          diagnosedFilePath $ directionDestination $ deploymentDirections d
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
