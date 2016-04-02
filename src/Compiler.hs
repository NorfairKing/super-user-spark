module Compiler where

import           Compiler.Internal
import           Compiler.Types
import           Control.Monad              (when)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (find, stripPrefix)
import           Language.Types
import           Parser
import           PreCompiler
import           System.FilePath            (takeDirectory, (</>))
import           Types

compileJob :: CardFileReference -> Sparker [Deployment]
compileJob cr@(CardFileReference root _) = go "" cr
  where
    go :: FilePath -> CardFileReference -> Sparker [Deployment]
    go base (CardFileReference fp mcn) = do
        sf <- parseFile fp
        let scope = sparkFileCards sf
        unit <- case mcn of
                Nothing -> case scope of
                    [] -> throwError $ CompileError $ "No cards found for compilation in file:" ++ fp
                    (first:_) -> return first
                Just (CardNameReference name) -> do
                    case find (\c -> cardName c == name) scope of
                            Nothing   -> throwError $ CompileError $ unwords ["Card", name, "not found for compilation."]
                            Just cu -> return cu

        -- Inject base outofDir
        let injected = injectBase unit

        -- Precompile checks
        let pces = preCompileChecks injected
        when (not . null $ pces) $ throwError $ PreCompileError pces

        -- Compile this unit
        (deps, crfs) <- embedPureCompiler $ compileUnit injected

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
    out <- asks conf_compile_output
    liftIO $ do
        let bs = encodePretty deps
        case out of
            Nothing -> BS.putStrLn bs
            Just fp -> BS.writeFile fp bs

inputCompiled :: FilePath -> Sparker [Deployment]
inputCompiled fp = do
    bs <- liftIO $ BS.readFile fp
    case eitherDecode bs of
        Left err        -> throwError $ CompileError $ "Something went wrong while deserialising json data: " ++ err
        Right ds        -> return ds


