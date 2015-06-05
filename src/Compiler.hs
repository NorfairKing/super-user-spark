module Compiler where

import           Control.Monad             (unless)
import           Control.Monad.State.Lazy  (get, gets, modify, put, runStateT)
import           Control.Monad.Writer.Lazy (runWriter, tell)

import           Types

compile :: [Declaration] -> [Deployment]
compile ds = dps
  where ((_,_),dps) = runSparkCompiler (initialState ds) compileDeployments

runSparkCompiler :: CompilerState -> SparkCompiler a -> ((a,CompilerState), [Deployment])
runSparkCompiler s func = runWriter (runStateT func s)

initialState :: [Declaration] -> CompilerState
initialState ds = CompilerState {
        state_declarations_left = ds
    ,   state_deployment_kind_override = UnspecifiedDeployment
    ,   state_into_prefix = ""
    ,   state_outof_prefix = ""
    }

pop :: SparkCompiler Declaration
pop = fmap head $ gets state_declarations_left

done :: SparkCompiler Bool
done = fmap null $ gets state_declarations_left

compileDeployments :: SparkCompiler ()
compileDeployments = do
    d <- done
    if d
    then return ()
    else processDeclaration

add :: Deployment -> SparkCompiler ()
add dep = tell [dep]

processDeclaration :: SparkCompiler ()
processDeclaration = do
    dec <- pop
    case dec of
        Deploy src dst kind -> do
            override <- gets state_deployment_kind_override
            let resultKind = if override == UnspecifiedDeployment
                then kind
                else override
            into <- gets state_into_prefix
            outof <- gets state_outof_prefix
            let source = outof ++ src
            let destination = into ++ dst
            let dep = case kind of
                    LinkDeployment -> Link source destination
                    CopyDeployment -> Copy source destination
                    UnspecifiedDeployment -> Link source destination -- Link is the default
            add dep
        SparkOff st -> undefined
        IntoDir dir -> modify (\s -> s {state_into_prefix = dir} )
        OutofDir dir -> modify (\s -> s {state_into_prefix = dir} )
        Block ds -> do
            before <- get
            modify (\s -> s {state_declarations_left = ds})
            compileDeployments
            put before
