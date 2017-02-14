{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (find, stripPrefix)
import System.FilePath (takeDirectory, (</>))

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
    CompileAssignment <$$> pure (readEither compileCardRef) <**>
    deriveCompileSettings compileFlags

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

decideCardToCompile :: CardFileReference -> [Card] -> Either CompileError Card
decideCardToCompile (CardFileReference fp mcn) scope =
    case mcn of
        Nothing ->
            case scope of
                [] ->
                    Left $
                    DuringCompilationError $
                    "No cards found for compilation in file:" ++ fp
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

injectBase :: FilePath -> Card -> Card
injectBase base c@(Card name s)
    | null base = c
    | otherwise = Card name $ Block [OutofDir base, s]

composeBases :: FilePath -> FilePath -> FilePath -> FilePath
composeBases _ base_ [] = base_
composeBases root _ base2 = takeDirectory (stripRoot root base2)

stripRoot :: FilePath -> FilePath -> FilePath
stripRoot root orig =
    case stripPrefix (takeDirectory root) orig of
        Nothing -> orig
        Just ('/':new) -> new
        Just new -> new

compileJob :: CardFileReference -> ImpureCompiler [Deployment]
compileJob cr@(CardFileReference root _) = compileJobWithRoot root "" cr

compileJobWithRoot :: FilePath
                   -> FilePath
                   -> CardFileReference
                   -> ImpureCompiler [Deployment]
compileJobWithRoot root base cfr@(CardFileReference fp _) = do
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
    let rcrfs = map (resolveCardReferenceRelativeTo fp) crfs
    restDeps <-
        fmap concat $
        forM rcrfs $ \rcrf ->
            case rcrf of
                (CardFile cfr_@(CardFileReference base2 _)) ->
                    compileJobWithRoot root (composeBases root base base2) cfr_
                (CardName cnr) ->
                    compileJobWithRoot
                        root
                        base
                        (CardFileReference fp $ Just cnr)
    return $ deps ++ restDeps

resolveCardReferenceRelativeTo :: FilePath -> CardReference -> CardReference
resolveCardReferenceRelativeTo fp (CardFile (CardFileReference cfp mcn)) =
    CardFile $ CardFileReference (takeDirectory fp </> cfp) mcn
resolveCardReferenceRelativeTo _ cn = cn

compilerParse :: FilePath -> ImpureCompiler SparkFile
compilerParse fp = do
    esf <- liftIO $ parseAbsFile fp >>= parseFile
    case esf of
        Left pe -> throwError $ CompileParseError pe
        Right sf_ -> pure sf_

embedPureCompiler :: PureCompiler a -> ImpureCompiler a
embedPureCompiler = withExceptT id . mapExceptT (mapReaderT idToIO)
  where
    idToIO :: Identity a -> IO a
    idToIO = return . runIdentity

outputCompiled :: [Deployment] -> ImpureCompiler ()
outputCompiled deps = do
    out <- asks compileOutput
    liftIO $ do
        let bs = encodePretty deps
        case out of
            Nothing -> putStrLn bs
            Just fp -> writeFile fp bs

inputCompiled :: FilePath -> ImpureCompiler [Deployment]
inputCompiled fp = do
    bs <- liftIO $ BS.readFile fp
    case eitherDecode bs of
        Left err ->
            throwError $
            DuringCompilationError $
            "Something went wrong while deserialising json data: " ++ err
        Right ds -> return ds
