module Compiler.Internal where

import           Compiler.Types
import           Data.List        (find, stripPrefix)
import qualified Parser           as P
import           Parser.Types
import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.FilePath  (normalise, takeDirectory, (</>))
import           Types

compile :: CompileUnit -> CompileScope -> Sparker [Deployment]
compile unit scope = do
    initial <- initialState unit scope
    ((_,_),dps) <- runSparkCompiler initial compileDeployments
    return dps

initialState :: CompileUnit -> CompileScope -> Sparker CompilerState
initialState cu scope = do
    currentDir <- liftIO getCurrentDirectory
    override <- asks conf_compile_kind
    let statement = card_content . unitCard $ cu
    return $ CompilerState {
        state_current_unit = cu
    ,   state_current_directory = currentDir </> takeDirectory (unitFilePath cu)
    ,   state_scope = scope
    ,   state_declarations_left = [statement]
    ,   state_deployment_kind_override = override
    ,   state_into = ""
    ,   state_outof_prefix = []
    }

compileDeployments :: SparkCompiler ()
compileDeployments = do
    d <- done
    if d
    then return ()
    else processDeclaration >> compileDeployments

pop :: SparkCompiler Declaration
pop = do
    dec <- gets state_declarations_left
    modify (\s -> s {state_declarations_left = tail dec})
    return $ head dec

done :: SparkCompiler Bool
done = fmap null $ gets state_declarations_left

add :: Deployment -> SparkCompiler ()
add dep = tell [dep]

addAll :: [Deployment] -> SparkCompiler ()
addAll = tell

processDeclaration :: SparkCompiler ()
processDeclaration = do
    dec <- pop
    case dec of
        Deploy src dst kind -> do
            override <- gets state_deployment_kind_override
            superOverride <- asks conf_compile_override
            let resultKind = case (superOverride, override, kind) of
                    (Nothing, Nothing, Nothing) -> LinkDeployment
                    (Nothing, Nothing, Just k ) -> k
                    (Nothing, Just o , _      ) -> o
                    (Just o , _      , _      ) -> o

            dir <- gets state_current_directory

            outof <- gets state_outof_prefix
            into <- gets state_into

            let alts = map normalise . resolvePrefix $ [Literal dir] ++ outof ++ [sources src]
            let dest = normalise $ into </> dst

            alternates <- mapM stripHome alts
            destination <- stripHome dest

            add $ Put alternates destination resultKind
        SparkOff st -> do
            case st of
                CardFile (CardFileReference file mn) -> do
                    dir <- gets state_current_directory
                    newCards <- fmap P.resolveUnits $ liftSparker $ P.parseFile $ dir </> file
                    oldCards <- gets state_scope
                    let allCards = oldCards ++ newCards
                    nextCard <- case mn of
                                    Nothing -> return $ head newCards
                                    Just (CardNameReference name) ->
                                        case find (\c -> (card_name . unitCard) c == name) allCards of
                                            Nothing -> throwError $ CompileError "card not found" -- FIXME this is unsafe.
                                            Just c -> return c
                    newDeclarations <- liftSparker $ compile nextCard newCards
                    modify (\s -> s {state_scope = allCards})
                    addAll newDeclarations

                CardName (CardNameReference name) -> do
                    allCards <- gets state_scope
                    case find (\c -> (card_name . unitCard) c == name) allCards of
                        Nothing -> throwError $ CompileError "card not found" -- FIXME this is unsafe.
                        Just cu -> do
                            let statement = card_content . unitCard $ cu
                            before <- get
                            modify (\s -> s { state_declarations_left = [statement]
                                            , state_current_unit = cu})
                            compileDeployments
                            put before
        IntoDir dir -> do
            ip <- gets state_into
            if null ip
            then modify (\s -> s {state_into = dir} )
            else modify (\s -> s {state_into = ip </> dir} )
        OutofDir dir -> do
            op <- gets state_outof_prefix
            modify (\s -> s {state_outof_prefix = op ++ [Literal dir]})
        DeployKindOverride kind -> modify (\s -> s {state_deployment_kind_override = Just kind })
        Block ds -> do
            before <- get
            modify (\s -> s {state_declarations_left = ds})
            compileDeployments
            put before
        Alternatives ds -> do
            op <- gets state_outof_prefix
            modify (\s -> s {state_outof_prefix = op ++ [Alts ds]})

liftSparker :: Sparker a -> SparkCompiler a
liftSparker = lift . lift


sources :: FilePath -> PrefixPart
sources fp@('.':f) = Alts [fp, f]
sources fp = Literal fp

stripHome :: FilePath -> SparkCompiler FilePath
stripHome fp = do
    home <- liftIO $ getHomeDirectory
    return $ case home `stripPrefix` fp of
                Nothing -> fp
                Just stripped -> "~" ++ stripped


resolvePrefix :: CompilerPrefix -> [FilePath]
resolvePrefix [] = []
resolvePrefix [Literal s] = [s]
resolvePrefix [Alts ds] = ds
resolvePrefix ((Literal s):ps) = do
    rest <- resolvePrefix ps
    return $ s </> rest
resolvePrefix ((Alts as):ps) = do
    a <- as
    rest <- resolvePrefix ps
    return $ a </> rest

