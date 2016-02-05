module Compiler.Internal where

import           Compiler.Types
import           Compiler.Utils
import           Parser.Types
import           System.FilePath ((</>))
import           Types
import           Utils

preCompileChecks :: Card -> [PreCompileError]
preCompileChecks c = runIdentity $ execWriterT $ cleanCardCheck c

dirty :: String -> Precompiler ()
dirty s = tell ["Precompilation check failed: " ++ s]

cleanCardCheck :: Card -> Precompiler ()
cleanCardCheck (Card name d) = do
    cleanCardNameCheck name
    cleanDeclarationCheck d

cleanDeclarationCheck :: Declaration -> Precompiler ()
cleanDeclarationCheck (Deploy src dst _) = do
    cleanFilePathCheck src
    cleanFilePathCheck dst

cleanDeclarationCheck (SparkOff cr) = cleanCardReferenceCheck cr
cleanDeclarationCheck (IntoDir dir) = cleanFilePathCheck dir
cleanDeclarationCheck (OutofDir dir) = cleanFilePathCheck dir
cleanDeclarationCheck (DeployKindOverride _) = return () -- Nothing can go wrong.
cleanDeclarationCheck (Alternatives fs) = mapM_ cleanFilePathCheck fs
cleanDeclarationCheck (Block ds) = mapM_ cleanDeclarationCheck ds

cleanCardReferenceCheck :: CardReference -> Precompiler ()
cleanCardReferenceCheck (CardFile cfr) = cleanCardFileReferenceCheck cfr
cleanCardReferenceCheck (CardName cnr) = cleanCardNameReferenceCheck cnr

cleanCardFileReferenceCheck :: CardFileReference -> Precompiler ()
cleanCardFileReferenceCheck (CardFileReference fp mcnr) = do
    cleanFilePathCheck fp
    case mcnr of
        Nothing -> return ()
        Just cnr -> cleanCardNameReferenceCheck cnr

cleanCardNameReferenceCheck :: CardNameReference -> Precompiler ()
cleanCardNameReferenceCheck (CardNameReference cn) = cleanCardNameCheck cn

cleanCardNameCheck :: CardName -> Precompiler ()
cleanCardNameCheck n
    | containsNewline n = dirty $ "Card name contains newline character(s): " ++ n
    | otherwise = return ()

cleanFilePathCheck :: FilePath -> Precompiler ()
cleanFilePathCheck [] = dirty "Empty filepath"
cleanFilePathCheck fp
    | containsNewline fp =
        dirty $ "Filepath contains newline character(s): " ++ fp
    | containsMultipleConsequtiveSlashes fp =
        dirty $ "Filepath contains multiple consequtive slashes: " ++ fp
    | otherwise = return ()

compileUnit :: Card -> PureCompiler ([Deployment], [CardReference])
compileUnit card = do
    initSt <- initialState
    execWriterT $ evalStateT (compileDecs [card_content card]) initSt

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

