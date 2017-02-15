{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-
    The Compiler is responsible for transforming an AST into a list of
    deployments. A deployment knows about the possible sources, the
    destination, and how to deploy a source to a destination.

    Everything that the compiler does needs to be independent of the host
    system because compilation could have happened independently of deployment.

    As such, raw deployments still contain references to variables such as:
    - Environment variables
    - The home directory: @~@
-}
module SuperUserSpark.Compiler where

import Import hiding ((</>))

import Control.Exception (try)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (find)

import SuperUserSpark.Compiler.Internal
import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types
import SuperUserSpark.OptParse.Types
import SuperUserSpark.Parser
import SuperUserSpark.PreCompiler
import SuperUserSpark.Utils

compileFromArgs :: CompileArgs -> IO ()
compileFromArgs ca = do
    errOrrAss <- compileAssignment ca
    case errOrrAss of
        Left ce -> die $ unwords ["Failed to build compile assignment:", ce]
        Right ass -> compile ass

compileAssignment :: CompileArgs -> IO (Either String CompileAssignment)
compileAssignment CompileArgs {..} =
    CompileAssignment <$$> (parseStrongCardFileReference compileCardRef) <**>
    deriveCompileSettings compileFlags

resolveFile'Either :: FilePath -> IO (Either String (Path Abs File))
resolveFile'Either fp = do
    errOrSp <- try $ resolveFile' fp
    pure $ left (show :: PathParseException -> String) $ errOrSp

parseStrongCardFileReference :: FilePath
                             -> IO (Either String StrongCardFileReference)
parseStrongCardFileReference fp =
    (\sfp -> StrongCardFileReference sfp Nothing) <$$> resolveFile'Either fp

deriveCompileSettings :: CompileFlags -> IO (Either String CompileSettings)
deriveCompileSettings CompileFlags {..} =
    CompileSettings <$$>
    (case compileFlagOutput of
         Nothing -> pure $ pure Nothing
         Just f -> do
             af <-
                 left (show :: PathParseException -> String) <$>
                 try (resolveFile' f)
             pure $ Just <$> af) <**>
    (pure $
     case compileDefaultKind of
         Nothing -> Right LinkDeployment
         Just s -> readEither s) <**>
    (pure $
     case compileKindOverride of
         Nothing -> Right Nothing
         Just s -> readEither s)

compile :: CompileAssignment -> IO ()
compile CompileAssignment {..} = do
    errOrDone <-
        runReaderT
            (runExceptT $ compileJob compileCardReference >>= outputCompiled)
            compileSettings
    case errOrDone of
        Left ce -> die $ formatCompileError ce
        Right () -> pure ()

formatCompileError :: CompileError -> String
formatCompileError (CompileParseError s) = unlines ["Parse failed:", show s]
formatCompileError (PreCompileErrors ss) =
    unlines $ "Precompilation checks failed:" : map show ss
formatCompileError (DuringCompilationError s) =
    unlines ["Compilation failed:", s]

decideCardToCompile :: StrongCardFileReference
                    -> [Card]
                    -> Either CompileError Card
decideCardToCompile (StrongCardFileReference fp mcn) scope =
    case mcn of
        Nothing ->
            case scope of
                [] ->
                    Left $
                    DuringCompilationError $
                    unwords
                        [ "No cards found for compilation in file:"
                        , toFilePath fp
                        ]
                            -- TODO more detailed error here
                (fst_:_) -> pure fst_
        Just (CardNameReference name) -> do
            case find (\c -> cardName c == name) scope of
                Nothing ->
                    Left $
                    DuringCompilationError $
                    unwords ["Card", name, "not found for compilation."] -- TODO more detailed error here
                Just cu -> return cu

throwEither :: Either CompileError a -> ImpureCompiler a
throwEither (Left e) = throwError e
throwEither (Right a) = pure a

injectBase :: Maybe (Path Rel Dir) -> Card -> Card
injectBase Nothing c = c
injectBase (Just base) (Card name s) =
    Card name $ Block [OutofDir $ toFilePath base, s]

compileJob :: StrongCardFileReference -> ImpureCompiler [RawDeployment]
compileJob cr@(StrongCardFileReference root _) =
    compileJobWithRoot root Nothing cr

compileJobWithRoot
    :: Path Abs File
    -> Maybe (Path Rel Dir)
    -> StrongCardFileReference
    -> ImpureCompiler [RawDeployment]
compileJobWithRoot root base cfr@(StrongCardFileReference fp _) = do
    sf <- compilerParse fp
    unit <- throwEither $ decideCardToCompile cfr $ sparkFileCards sf
    -- Inject base outofDir
    let injected = injectBase base unit
    -- Precompile checks
    let pces = preCompileChecks injected
    when (not . null $ pces) $ throwError $ PreCompileErrors pces
    -- Compile this unit
    (deps, crfs) <- embedPureCompiler $ compileUnit injected
    -- Compile the rest of the units
    rcrfs <- mapM (resolveCardReferenceRelativeTo fp) crfs
    restDeps <-
        fmap concat $
        forM rcrfs $ \rcrf ->
            case rcrf of
                (StrongCardFile cfr_@(StrongCardFileReference base2 _)) -> do
                    let (newRoot, newBase) =
                            case stripDir (parent root) (parent base2) of
                                Nothing -> (base2, Nothing)
                                Just d -> (root, Just d)
                    compileJobWithRoot newRoot newBase cfr_
                (StrongCardName cnr) ->
                    compileJobWithRoot
                        root
                        base
                        (StrongCardFileReference fp $ Just cnr)
    return $ deps ++ restDeps

resolveCardReferenceRelativeTo :: Path Abs File
                               -> CardReference
                               -> ImpureCompiler StrongCardReference
resolveCardReferenceRelativeTo fp (CardFile (CardFileReference cfp mcn)) = do
    nfp <- liftIO $ resolveFile (parent fp) cfp
    pure $ StrongCardFile $ StrongCardFileReference nfp mcn
resolveCardReferenceRelativeTo _ (CardName cnr) = pure $ StrongCardName cnr

compilerParse :: Path Abs File -> ImpureCompiler SparkFile
compilerParse fp = do
    esf <- liftIO $ parseFile fp
    case esf of
        Left pe -> throwError $ CompileParseError pe
        Right sf_ -> pure sf_

embedPureCompiler :: PureCompiler a -> ImpureCompiler a
embedPureCompiler = withExceptT id . mapExceptT (mapReaderT idToIO)
  where
    idToIO :: Identity a -> IO a
    idToIO = return . runIdentity

outputCompiled :: [RawDeployment] -> ImpureCompiler ()
outputCompiled deps = do
    out <- asks compileOutput
    liftIO $ do
        let bs = encodePretty deps
        case out of
            Nothing -> putStrLn bs
            Just fp -> writeFile fp bs

inputCompiled :: Path Abs File -> ImpureCompiler [RawDeployment]
inputCompiled fp = do
    bs <- readFile fp
    case eitherDecode bs of
        Left err ->
            throwError $
            DuringCompilationError $
            "Something went wrong while deserialising json data: " ++ err
        Right ds -> return ds
