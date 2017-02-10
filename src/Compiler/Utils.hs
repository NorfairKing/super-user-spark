module Compiler.Utils where

import Import

import Control.Monad.Writer
import Control.Monad.Reader

import Config.Types
import Compiler.Types
import Language.Types
import System.FilePath ((</>))
import Types

initialState :: PureCompiler CompilerState
initialState = do
    override <- asks conf_compile_kind
    return $
        CompilerState
        { state_deployment_kind_override = override
        , state_into = ""
        , state_outof_prefix = []
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
