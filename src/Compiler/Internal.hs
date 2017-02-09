module Compiler.Internal where

import Compiler.Types
import Compiler.Utils
import Language.Types
import System.FilePath ((</>))
import Types

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
    let resultKind =
            case (superOverride, override, kind) of
                (Nothing, Nothing, Nothing) -> LinkDeployment
                (Nothing, Nothing, Just k) -> k
                (Nothing, Just o, _) -> o
                (Just o, _, _) -> o
    outof <- gets state_outof_prefix
    into <- gets state_into
    let alternates = resolvePrefix $ outof ++ [sources src]
    let destination = into </> dst
    addDeployment $ Put alternates destination resultKind
compileDec (SparkOff cr) = addCardRef cr
compileDec (IntoDir dir) = do
    ip <- gets state_into
    if null ip
        then modify (\s -> s {state_into = dir})
        else modify (\s -> s {state_into = ip </> dir})
compileDec (OutofDir dir) = do
    op <- gets state_outof_prefix
    modify (\s -> s {state_outof_prefix = op ++ [Literal dir]})
compileDec (DeployKindOverride kind) = do
    modify (\s -> s {state_deployment_kind_override = Just kind})
compileDec (Block ds) = do
    before <- get
    compileDecs ds
    put before
compileDec (Alternatives ds) = do
    op <- gets state_outof_prefix
    modify (\s -> s {state_outof_prefix = op ++ [Alts ds]})
