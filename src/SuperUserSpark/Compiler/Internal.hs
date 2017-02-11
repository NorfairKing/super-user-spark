module SuperUserSpark.Compiler.Internal where

import Import

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import SuperUserSpark.Compiler.Types
import SuperUserSpark.CoreTypes
import SuperUserSpark.Compiler.Utils
import SuperUserSpark.Config.Types
import SuperUserSpark.Language.Types
import System.FilePath ((</>))

compileUnit :: Card -> PureCompiler ([Deployment], [CardReference])
compileUnit card = do
    initSt <- initialState
    execWriterT $ evalStateT (compileDecs [cardContent card]) initSt

compileDecs :: [Declaration] -> InternalCompiler ()
compileDecs = mapM_ compileDec

compileDec :: Declaration -> InternalCompiler ()
compileDec (Deploy src dst kind) = do
    override <- gets stateDeploymentKindOverride
    superOverride <- asks confCompileOverride
    let resultKind =
            case (superOverride, override, kind) of
                (Nothing, Nothing, Nothing) -> LinkDeployment
                (Nothing, Nothing, Just k) -> k
                (Nothing, Just o, _) -> o
                (Just o, _, _) -> o
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
    modify (\s -> s {stateDeploymentKindOverride = Just kind})
compileDec (Block ds) = do
    before <- get
    compileDecs ds
    put before
compileDec (Alternatives ds) = do
    op <- gets stateOutof_prefix
    modify (\s -> s {stateOutof_prefix = op ++ [Alts ds]})
