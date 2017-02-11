module SuperUserSpark.Compiler.Utils where

import Import

import Control.Monad.Writer
import Control.Monad.Reader

import SuperUserSpark.Config.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Types
import System.FilePath ((</>))

initialState :: PureCompiler CompilerState
initialState = do
    override <- asks confCompileKind
    return $
        CompilerState
        { stateDeploymentKindOverride = override
        , stateInto = ""
        , stateOutof_prefix = []
        }

addDeployment :: Deployment -> InternalCompiler ()
addDeployment d = tell ([d], [])

addCardRef :: CardReference -> InternalCompiler ()
addCardRef c = tell ([], [c])

sources :: FilePath -> PrefixPart
sources fp@('.':f) = Alts [fp, f]
sources fp = Literal fp

resolvePrefix :: CompilerPrefix -> [FilePath]
resolvePrefix [] = []
resolvePrefix [Literal s] = [s]
resolvePrefix [Alts ds] = ds
resolvePrefix ((Literal s):ps) = do
    rest <- resolvePrefix ps
    return $ s </> rest
resolvePrefix ((Alts as):ps) = do
    a <- as
    rest <- resolvePrefix ps
    return $ a </> rest
