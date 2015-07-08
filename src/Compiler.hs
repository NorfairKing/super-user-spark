module Compiler where

import           Data.List        (find, isPrefixOf)
import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.FilePath  (takeDirectory, (</>))


import qualified Parser           as P
import           Types

compile :: Card -> [Card] -> Sparker [Deployment]
compile card allCards = do
    initial <- initialState card allCards
    ((_,_),dps) <- runSparkCompiler initial compileDeployments
    return dps

initialState :: Card -> [Card] -> Sparker CompilerState
initialState c@(Card _ fp ds) cds = do
    currentDir <- liftIO getCurrentDirectory
    return $ CompilerState {
        state_current_card = c
    ,   state_current_directory = currentDir </> takeDirectory fp
    ,   state_all_cards = cds
    ,   state_declarations_left = ds
    ,   state_deployment_kind_override = Nothing
    ,   state_into_prefix = ""
    ,   state_outof_prefix = ""
    ,   state_alternatives = [""]
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

addAll :: [Deployment] -> SparkCompiler ()
addAll = tell

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
                    (Nothing, Nothing) -> LinkDeployment
                    (Just o, Nothing) -> o
                    (_, Just k) -> k

            alternates <- gets state_alternatives
            dir <- gets state_current_directory

            outof <- gets state_outof_prefix
            into <- gets state_into_prefix

            let alts = map (\alt -> dir </> alt </> outof </> src) alternates
            destination <- replaceHome $ into </> dst

            add $ Put alts destination resultKind

        SparkOff st -> do
            case st of
                CardRepo _ -> throwError $ UnpredictedError "not yet implemented"
                CardFile (CardFileReference file mn) -> do
                    dir <- gets state_current_directory
                    newCards <- liftSparker $ P.parseFile $ dir </> file
                    oldCards <- gets state_all_cards
                    let allCards = oldCards ++ newCards
                    nextCard <- case mn of
                                    Nothing -> return $ head newCards
                                    Just (CardNameReference name) ->
                                        case find (\c -> card_name c == name) allCards of
                                            Nothing -> throwError $ CompileError "card not found" -- FIXME this is unsafe.
                                            Just c -> return c
                    newDeclarations <- liftSparker $ compile nextCard newCards
                    modify (\s -> s {state_all_cards = allCards})
                    addAll newDeclarations
                CardName (CardNameReference name) -> do
                    allCards <- gets state_all_cards
                    case find (\c -> card_name c == name) allCards of
                        Nothing -> throwError $ CompileError "card not found" -- FIXME this is unsafe.
                        Just c@(Card _ _ dcs) -> do
                            before <- get
                            modify (\s -> s {state_declarations_left = dcs
                                            ,state_current_card = c})
                            compileDeployments
                            put before



        IntoDir dir -> do
            ip <- gets state_into_prefix
            if null ip
            then modify (\s -> s {state_into_prefix = dir} )
            else modify (\s -> s {state_into_prefix = ip </> dir} )
        OutofDir dir -> do
            op <- gets state_outof_prefix
            if null op
            then modify (\s -> s {state_outof_prefix = dir} )
            else modify (\s -> s {state_outof_prefix = op </> dir} )
        DeployKindOverride kind -> modify (\s -> s {state_deployment_kind_override = Just kind })
        Block ds -> do
            before <- get
            modify (\s -> s {state_declarations_left = ds})
            compileDeployments
            put before
        Alternatives ds -> modify (\s -> s {state_alternatives = ds})

liftSparker :: Sparker a -> SparkCompiler a
liftSparker = lift . lift
