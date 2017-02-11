module SuperUserSpark.Compiler.Internal where

import Import

import System.FilePath ((</>))

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Compiler.Utils
import SuperUserSpark.Language.Types

compileUnit :: Card -> PureCompiler ([Deployment], [CardReference])
compileUnit card =
    execWriterT $ evalStateT (compileDecs [cardContent card]) initialState

compileDecs :: [Declaration] -> InternalCompiler ()
compileDecs = mapM_ compileDec

compileDec :: Declaration -> InternalCompiler ()
compileDec (Deploy src dst kind) = do
    defaultKind <- asks compileDefaultKind
    localOverride <- gets stateDeploymentKindLocalOverride
    superOverride <- asks compileKindOverride
    let resultKind =
            case msum [superOverride, localOverride, kind] of
                Nothing -> defaultKind
                Just k -> k
    outof <- gets stateOutof_prefix
    into <- gets stateInto
    let alternates = resolvePrefix $ outof ++ [sources src]
    let destination = into </> dst
    addDeployment $ Put alternates destination resultKind
compileDec (SparkOff cr) = addCardRef cr
compileDec (IntoDir dir) = do
    ip <- gets stateInto
    if null ip
        then modify (\s -> s {stateInto = dir})
        else modify (\s -> s {stateInto = ip </> dir})
compileDec (OutofDir dir) = do
    op <- gets stateOutof_prefix
    modify (\s -> s {stateOutof_prefix = op ++ [Literal dir]})
compileDec (DeployKindOverride kind) = do
    modify (\s -> s {stateDeploymentKindLocalOverride = Just kind})
compileDec (Block ds) = do
    before <- get
    compileDecs ds
    put before
compileDec (Alternatives ds) = do
    op <- gets stateOutof_prefix
    modify (\s -> s {stateOutof_prefix = op ++ [Alts ds]})
