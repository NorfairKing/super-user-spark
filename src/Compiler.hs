module Compiler where

import           Codec.Compression.GZip     (bestCompression, compressLevel,
                                             compressWith, decompress,
                                             defaultCompressParams)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (find, stripPrefix)
import           System.Directory           (getCurrentDirectory,
                                             getHomeDirectory)
import           System.FilePath            (normalise, takeDirectory, (</>))

import           Compiler.Types
import qualified Parser                     as P
import           Parser.Types
import           Types
import           Utils

compileRef :: [Card] -> Maybe CardNameReference -> Sparker [Deployment]
compileRef cs mcnr = do
    firstCard <- case mcnr of
            Nothing -> if null cs
                        then throwError $ CompileError "No cards found for compilation."
                        else return $ head cs
            Just (CardNameReference name) -> do
                case find (\c -> card_name c == name) cs of
                        Nothing   -> throwError $ CompileError $ unwords ["Card", name, "not found for compilation."]
                        Just card -> return card
    compile firstCard cs


compile :: Card -> [Card] -> Sparker [Deployment]
compile card allCards = do
    initial <- initialState card allCards
    ((_,_),dps) <- runSparkCompiler initial compileDeployments
    return dps

outputCompiled :: [Deployment] -> Sparker ()
outputCompiled deps = do
    form <- asks conf_compile_format
    out <- asks conf_compile_output
    case form of
        FormatBinary -> do
            case out of
                Nothing -> liftIO $ BS.putStrLn $ compressWith compressionParams $ encode deps
                Just fp -> liftIO $ BS.writeFile fp $ compressWith compressionParams $ encode deps
        FormatText -> do
            let str = unlines $ map show deps
            case out of
                Nothing -> liftIO $ putStrLn str
                Just fp -> liftIO $ writeFile fp str
        FormatJson -> do
            let bs = encodePretty deps
            case out of
                Nothing -> liftIO $ BS.putStrLn bs
                Just fp -> liftIO $ BS.writeFile fp bs

        FormatStandalone -> notImplementedYet
  where
    compressionParams = defaultCompressParams {compressLevel = bestCompression}

inputCompiled :: FilePath -> Sparker [Deployment]
inputCompiled fp = do
    form <- asks conf_compile_format
    case form of
        FormatBinary -> do
            content <- liftIO $ BS.readFile fp
            case decodeOrFail $ decompress content of
                Left (_,_,err)    -> throwError $ CompileError $ "Something went wrong while deserialising binary data: " ++ err
                Right (_,_,deps)  -> return deps
        FormatText -> do
            str <- liftIO $ readFile fp
            return $ map read $ lines str
        FormatJson -> do
            bs <- liftIO $ BS.readFile fp
            case eitherDecode bs of
                Left err        -> throwError $ CompileError $ "Something went wrong while deserialising json data: " ++ err
                Right ds        -> return ds

        FormatStandalone -> throwError $ CompileError "You're not supposed to use standalone compiled deployments in any other way than by executing it."


initialState :: Card -> [Card] -> Sparker CompilerState
initialState c@(Card _ fp statement) cds = do
    currentDir <- liftIO getCurrentDirectory
    override <- asks conf_compile_kind
    return $ CompilerState {
        state_current_card = c
    ,   state_current_directory = currentDir </> takeDirectory fp
    ,   state_all_cards = cds
    ,   state_declarations_left = [statement]
    ,   state_deployment_kind_override = override
    ,   state_into = ""
    ,   state_outof_prefix = []
    }

-- Compiler

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

sources :: FilePath -> PrefixPart
sources fp@('.':f) = Alts [fp, f]
sources fp = Literal fp

stripHome :: FilePath -> SparkCompiler FilePath
stripHome fp = do
    home <- liftIO $ getHomeDirectory
    return $ case home `stripPrefix` fp of
                Nothing -> fp
                Just stripped -> "~" ++ stripped

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
                        Just c@(Card _ _ statement) -> do
                            before <- get
                            modify (\s -> s {state_declarations_left = [statement]
                                            ,state_current_card = c})
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
