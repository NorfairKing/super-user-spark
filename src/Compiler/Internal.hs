module Compiler.Internal where

import           Compiler.Types
import           Compiler.Utils
import           Language.Types
import           System.FilePath ((</>))
import           Types
import           Utils

preCompileChecks :: Card -> [PreCompileError]
preCompileChecks c = runIdentity $ execWriterT $ cleanCard c

dirty :: String -> Precompiler ()
dirty s = tell ["Precompilation check failed: " ++ s]

cleanCard :: Card -> Precompiler ()
cleanCard (Card name d) = do
    cleanCardName name
    cleanDeclaration d

cleanDeclaration :: Declaration -> Precompiler ()
cleanDeclaration (Deploy src dst _) = do
    cleanFilePath src
    cleanFilePath dst

cleanDeclaration (SparkOff cr) = cleanCardReference cr
cleanDeclaration (IntoDir dir) = cleanFilePath dir
cleanDeclaration (OutofDir dir) = cleanFilePath dir
cleanDeclaration (DeployKindOverride _) = return () -- Nothing can go wrong.
cleanDeclaration (Alternatives fs) = mapM_ cleanFilePath fs
cleanDeclaration (Block ds) = mapM_ cleanDeclaration ds

cleanCardReference :: CardReference -> Precompiler ()
cleanCardReference (CardFile cfr) = cleanCardFileReference cfr
cleanCardReference (CardName cnr) = cleanCardNameReference cnr

cleanCardFileReference :: CardFileReference -> Precompiler ()
cleanCardFileReference (CardFileReference fp mcnr) = do
    cleanFilePath fp
    case mcnr of
        Nothing -> return ()
        Just cnr -> cleanCardNameReference cnr

cleanCardNameReference :: CardNameReference -> Precompiler ()
cleanCardNameReference (CardNameReference cn) = cleanCardName cn

cleanCardName :: CardName -> Precompiler ()
cleanCardName n
    | containsNewline n = dirty $ "Card name contains newline character(s): " ++ n
    | otherwise = return ()

cleanFilePath :: FilePath -> Precompiler ()
cleanFilePath [] = dirty "Empty filepath"
cleanFilePath fp
    | containsNewline fp =
        dirty $ "Filepath contains newline character(s): " ++ fp
    | containsMultipleConsequtiveSlashes fp =
        dirty $ "Filepath contains multiple consequtive slashes: " ++ fp
    | otherwise = return ()

compileUnit :: Card -> PureCompiler ([Deployment], [CardReference])
compileUnit card = do
    initSt <- initialState
    execWriterT $ evalStateT (compileDecs [cardContent card]) initSt

compileDecs :: [Declaration] -> InternalCompiler ()
compileDecs = mapM_ compileDec


compileDec :: Declaration -> InternalCompiler ()
compileDec (Deploy src dst kind) = do
    override <- gets state_deployment_kind_override
    superOverride <- asks conf_compile_override
    let resultKind = case (superOverride, override, kind) of
            (Nothing, Nothing, Nothing) -> LinkDeployment
            (Nothing, Nothing, Just k ) -> k
            (Nothing, Just o , _      ) -> o
            (Just o , _      , _      ) -> o
    outof <- gets state_outof_prefix
    into <- gets state_into

    let alternates = resolvePrefix $ outof ++ [sources src]
    let destination = into </> dst

    addDeployment $ Put alternates destination resultKind


compileDec (SparkOff cr) = addCardRef cr


compileDec (IntoDir dir) = do
    ip <- gets state_into
    if null ip
    then modify (\s -> s {state_into = dir} )
    else modify (\s -> s {state_into = ip </> dir} )


compileDec (OutofDir dir) = do
    op <- gets state_outof_prefix
    modify (\s -> s {state_outof_prefix = op ++ [Literal dir]})


compileDec (DeployKindOverride kind) = do
    modify (\s -> s { state_deployment_kind_override = Just kind })


compileDec (Block ds) = do
    before <- get
    compileDecs ds
    put before


compileDec (Alternatives ds) = do
    op <- gets state_outof_prefix
    modify (\s -> s { state_outof_prefix = op ++ [Alts ds] })

