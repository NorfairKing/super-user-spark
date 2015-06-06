module Compiler where

import           Data.List        (find, isPrefixOf)
import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.FilePath  (takeDirectory, (<.>), (</>))


import           Types

compile :: Card -> [Card] -> Sparker [Deployment]
compile card allCards = do
    initial <- initialState card allCards
    ((_,_),dps) <- runSparkCompiler initial compileDeployments
    return dps

initialState :: Card -> [Card] -> Sparker CompilerState
initialState c@(Card name fp ds) cds = do
    currentDir <- liftIO getCurrentDirectory
    return $ CompilerState {
        state_current_card = c
    ,   state_current_directory = currentDir </> takeDirectory fp
    ,   state_all_cards = cds
    ,   state_declarations_left = ds
    ,   state_deployment_kind_override = UnspecifiedDeployment
    ,   state_into_prefix = ""
    ,   state_outof_prefix = ""
    }

pop :: SparkCompiler Declaration
pop = do
    dec <- gets state_declarations_left
    modify (\s -> s {state_declarations_left = tail dec})
    return $ head dec

done :: SparkCompiler Bool
done = fmap null $ gets state_declarations_left

compileDeployments :: SparkCompiler ()
compileDeployments = do
    d <- done
    if d
    then return ()
    else processDeclaration >> compileDeployments

add :: Deployment -> SparkCompiler ()
add dep = tell [dep]

replaceHome :: FilePath -> SparkCompiler FilePath
replaceHome path = do
    home <- liftIO getHomeDirectory
    return $ if "~" `isPrefixOf` path
        then home </> drop 2 path
        else path

processDeclaration :: SparkCompiler ()
processDeclaration = do
    dec <- pop
    case dec of
        Deploy src dst kind -> do
            override <- gets state_deployment_kind_override
            let resultKind = case (override, kind) of
                    (UnspecifiedDeployment, UnspecifiedDeployment) -> LinkDeployment
                    (o, UnspecifiedDeployment) -> o
                    (_, k) -> k

            dir <- gets state_current_directory

            outof <- gets state_outof_prefix
            into <- gets state_into_prefix

            let source = dir </> outof </> src
            destination <- replaceHome $ into </> dst

            let dep = case resultKind of
                    LinkDeployment -> Link source destination
                    CopyDeployment -> Copy source destination
                    UnspecifiedDeployment -> error "report to author if you see this message"

            add dep
        SparkOff st -> do
            case st of
                TargetGit repo -> error "not yet implemented"
                TargetCardName name -> do
                    allCards <- gets state_all_cards
                    case find (\(Card n _ _) -> n == name) allCards of
                        Nothing -> error "card not found" -- FIXME this is unsafe.
                        Just c@(Card cn fp dcs) -> do
                            before <- get
                            modify (\s -> s {state_declarations_left = dcs
                                            ,state_current_card = c})
                            compileDeployments
                            put before



        IntoDir dir -> modify (\s -> s {state_into_prefix = dir} )
        OutofDir dir -> modify (\s -> s {state_outof_prefix = dir} )
        DeployKindOverride kind -> modify (\s -> s {state_deployment_kind_override = kind })
        Block ds -> do
            before <- get
            modify (\s -> s {state_declarations_left = ds})
            compileDeployments
            put before


formatCards :: [Card] -> String
formatCards = unlines . map formatCard

formatCard :: Card -> String
formatCard (Card name _ decs) = unlines $ name:map formatDeclaration decs

formatDeclaration :: Declaration -> String
formatDeclaration (Deploy src dst kind) = unwords [src, formatDeployKind kind, dst]
formatDeclaration (SparkOff st) = unwords ["spark", formatSparkTarget st]
formatDeclaration (IntoDir dir) = unwords ["into", dir]
formatDeclaration (OutofDir dir) = unwords ["outof", dir]
formatDeclaration (DeployKindOverride kind) =  unwords ["kind", formatDeployKind kind]
formatDeclaration (Block ds) = unlines $ map formatDeclaration ds

formatDeployKind :: DeploymentKind -> String
formatDeployKind LinkDeployment = "l->"
formatDeployKind CopyDeployment = "c->"
formatDeployKind UnspecifiedDeployment = "->"

formatSparkTarget :: SparkTarget -> String
formatSparkTarget (TargetGit repo) = unwords ["git", show repo]
formatSparkTarget (TargetCardName name) = unwords ["card", name]

