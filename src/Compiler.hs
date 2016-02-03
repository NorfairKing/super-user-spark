module Compiler where

import           Codec.Compression.GZip     (bestCompression, compressLevel,
                                             compressWith, decompress,
                                             defaultCompressParams)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Binary                (decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (find)
import           System.FilePath            (takeDirectory, (</>))

import           Compiler.Internal
import           Compiler.Types
import           Parser
import           Parser.Types
import           Types
import           Utils

compileJob :: CompilerCardReference -> Sparker [Deployment]
compileJob (CardFileReference fp mcn) = do
    sf <- parseFile fp
    let scope = sparkFileCards sf
    unit <- case mcn of
            Nothing -> if null scope
                        then throwError $ CompileError $ "No cards found for compilation in file:" ++ fp
                        else return $ head scope
            Just (CardNameReference name) -> do
                case find (\c -> card_name c == name) scope of
                        Nothing   -> throwError $ CompileError $ unwords ["Card", name, "not found for compilation."]
                        Just cu -> return cu

    (deps, crfs) <- embedPureCompiler $ do
        preCompileChecks unit
        compileUnit unit
    restDeps <- fmap concat
                $ mapM compileCardReference
                $ map (resolveCardReferenceRelativeTo fp) crfs
    return $ deps ++ restDeps

  where
    compileCardReference :: CardReference -> Sparker [Deployment]
    compileCardReference (CardFile cfr) = compileJob cfr
    compileCardReference (CardName cnr) = compileJob (CardFileReference fp $ Just cnr)

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


