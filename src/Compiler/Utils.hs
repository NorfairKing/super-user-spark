module Compiler.Utils where

import           Compiler.Types
import           Parser.Types
import           Types

initialState :: PureCompiler CompilerState
initialState = do
    override <- asks conf_compile_kind
    return $ CompilerState {
        state_deployment_kind_override = override
    ,   state_into = ""
    ,   state_outof_prefix = []
    }

addDeployment :: Deployment ->  SuppliedCompiler ()
addDeployment d = tell ([d], [])

addCardRef :: CardReference -> SuppliedCompiler ()
addCardRef c = tell ([], [c])

