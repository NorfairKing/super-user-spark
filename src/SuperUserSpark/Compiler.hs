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

compileJob :: CardFileReference -> ImpureCompiler [Deployment]
compileJob cr@(CardFileReference root _) = go "" cr
  where
    go :: FilePath -> CardFileReference -> ImpureCompiler [Deployment]
    go base (CardFileReference fp mcn) = do
        sf <- compilerParse fp
        let scope = sparkFileCards sf
        unit <-
            case mcn of
                Nothing ->
                    case scope of
                        [] ->
                            throwError $
                            DuringCompilationError $
                            "No cards found for compilation in file:" ++ fp
                            -- TODO more detailed error here
                        (fst_:_) -> pure fst_
                Just (CardNameReference name) -> do
                    case find (\c -> cardName c == name) scope of
                        Nothing ->
                            throwError $
                            DuringCompilationError $
                            unwords ["Card", name, "not found for compilation."] -- TODO more detailed error here
                        Just cu -> return cu
        -- Inject base outofDir
        let injected = injectBase unit
        -- Precompile checks
        let pces = preCompileChecks injected
        when (not . null $ pces) $ throwError $ PreCompileErrors pces
        -- Compile this unit
        (deps, crfs) <- embedPureCompiler $ compileUnit injected
        -- Compile the rest of the units
        restDeps <-
            fmap concat $
            mapM compileCardReference $
            map (resolveCardReferenceRelativeTo fp) crfs
        return $ deps ++ restDeps
      where
        injectBase :: Card -> Card
        injectBase c@(Card name s)
            | null base = c
            | otherwise = Card name $ Block [OutofDir base, s]
        stripRoot :: FilePath -> FilePath
        stripRoot orig =
            case stripPrefix (takeDirectory root) orig of
                Nothing -> orig
                Just ('/':new) -> new
                Just new -> new
        composeBases :: FilePath -> FilePath -> FilePath
        composeBases base_ [] = base_
        composeBases _ base2 = takeDirectory (stripRoot base2)
        compileCardReference :: CardReference -> ImpureCompiler [Deployment]
        compileCardReference (CardFile cfr@(CardFileReference base2 _)) =
            go (composeBases base base2) cfr
        compileCardReference (CardName cnr) =
            go base (CardFileReference fp $ Just cnr)

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
