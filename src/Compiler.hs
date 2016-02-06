module Compiler where

import           Codec.Compression.GZip     (bestCompression, compressLevel,
                                             compressWith, decompress,
                                             defaultCompressParams)
import           Control.Monad              (when)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (find, stripPrefix)
import           Safe                       (headMay)
import           System.FilePath            (takeDirectory, (</>))

import           Compiler.Internal
import           Compiler.Types
import           Parser
import           Parser.Types
import           Types
import           Utils

compileJob :: CompilerCardReference -> Sparker [Deployment]
compileJob cr@(CardFileReference root _) = go "" cr
  where
    go :: FilePath -> CompilerCardReference -> Sparker [Deployment]
    go base (CardFileReference fp mcn) = do
        sf <- parseFile fp
        traceShow sf $ return ()
        let scope = sparkFileCards sf
        unit <- case mcn of
                Nothing -> case headMay scope of
                    Nothing -> throwError $ CompileError $ "No cards found for compilation in file:" ++ fp
                    Just first -> return first
                Just (CardNameReference name) -> do
                    case find (\c -> card_name c == name) scope of
                            Nothing   -> throwError $ CompileError $ unwords ["Card", name, "not found for compilation."]
                            Just cu -> return cu

        -- Inject base outofDir
        let injected = injectBase unit

        -- Precompile checks
        let pces = preCompileChecks injected
        when (not . null $ pces) $ throwError $ PreCompileError pces

        -- Compile this unit
        (deps, crfs) <- embedPureCompiler $ compileUnit $ traceShowId injected

        -- Compile the rest of the units
        restDeps <- fmap concat
                    $ mapM compileCardReference
                    $ map (resolveCardReferenceRelativeTo fp) crfs

        return $ deps ++ restDeps
      where

        injectBase :: Card -> Card
        injectBase c@(Card name s) | null base = c
                                   | otherwise = Card name $ Block [OutofDir base, s]

        stripRoot :: FilePath -> FilePath
        stripRoot orig = case stripPrefix (takeDirectory root) orig of
            Nothing -> orig
            Just ('/':new) -> new
            Just new -> new

        composeBases :: FilePath -> FilePath -> FilePath
        composeBases base [] = base
        composeBases _ base2 = takeDirectory (stripRoot base2)


        compileCardReference :: CardReference -> Sparker [Deployment]
        compileCardReference (CardFile cfr@(CardFileReference base2 _)) = go (composeBases base base2) cfr
        compileCardReference (CardName cnr) = go base (CardFileReference fp $ Just cnr)

resolveCardReferenceRelativeTo :: FilePath -> CardReference -> CardReference
resolveCardReferenceRelativeTo fp (CardFile (CardFileReference cfp mcn)) = CardFile $ CardFileReference (takeDirectory fp </> cfp) mcn
resolveCardReferenceRelativeTo _ cn = cn

embedPureCompiler :: PureCompiler a -> Sparker a
embedPureCompiler func = withExceptT CompileError $ mapExceptT (mapReaderT idToIO) func
  where
    idToIO :: Identity a -> IO a
    idToIO = return . runIdentity

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


