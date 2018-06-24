module SuperUserSpark.Compiler.Internal where

import Import hiding ((</>))

import System.FilePath ((</>))

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Compiler.Utils
import SuperUserSpark.Language.Types

compileUnit :: Card -> PureCompiler ([RawDeployment], [CardReference])
compileUnit card =
    execWriterT $ evalStateT (compileDecs [cardContent card]) initialState

compileDecs :: [Declaration] -> InternalCompiler ()
compileDecs = mapM_ compileDec

compileDec :: Declaration -> InternalCompiler ()
compileDec (Deploy src dst kind) = do
    defaultKind <- asks compileDefaultKind
    localOverride <- gets stateDeploymentKindLocalOverride
    superOverride <- asks compileKindOverride
    let resultKind = fromMaybe defaultKind
            $ msum [superOverride, localOverride, kind]
    outof <- gets stateOutofPrefix
    into <- gets stateInto
    let directions =
            Directions
            { directionSources = resolvePrefix $ outof ++ [sources src]
            , directionDestination = into </> dst
            }
    addDeployment $ Deployment directions resultKind
compileDec (SparkOff cr) = addCardRef cr
compileDec (IntoDir dir) = do
    ip <- gets stateInto
    if null ip
        then modify (\s -> s {stateInto = dir})
        else modify (\s -> s {stateInto = ip </> dir})
compileDec (OutofDir dir) = do
    op <- gets stateOutofPrefix
    modify (\s -> s {stateOutofPrefix = op ++ [Literal dir]})
compileDec (DeployKindOverride kind) =
    modify (\s -> s {stateDeploymentKindLocalOverride = Just kind})
compileDec (Block ds) = do
    before <- get
    compileDecs ds
    put before
compileDec (Alternatives ds) = do
    op <- gets stateOutofPrefix
    modify (\s -> s {stateOutofPrefix = op ++ [Alts ds]})
