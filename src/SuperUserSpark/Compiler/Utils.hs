module SuperUserSpark.Compiler.Utils where

import Import hiding ((</>))

import SuperUserSpark.Compiler.Types
import SuperUserSpark.Language.Types
import System.FilePath ((</>))

initialState :: CompilerState
initialState =
    CompilerState
    { stateDeploymentKindLocalOverride = Nothing
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
