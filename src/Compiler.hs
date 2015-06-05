module Compiler where

import           Control.Monad             (unless)
import           Control.Monad.State.Lazy  (get, gets, modify, put, runStateT)
import           Control.Monad.Writer.Lazy (runWriter, tell)
import           Data.List                 (find)
import           System.FilePath           (takeDirectory, (<.>), (</>))

import           Types

compile :: Card -> [Card] -> [Deployment]
compile card allCards = dps
  where ((_,_),dps) = runSparkCompiler (initialState card allCards) compileDeployments

runSparkCompiler :: CompilerState -> SparkCompiler a -> ((a,CompilerState), [Deployment])
runSparkCompiler s func = runWriter (runStateT func s)

initialState :: Card -> [Card] -> CompilerState
initialState c@(Card name fp ds) cds = CompilerState {
        state_current_card = c
    ,   state_current_directory = takeDirectory fp
    ,   state_all_cards = cds
    ,   state_declarations_left = ds
    ,   state_deployment_kind_override = UnspecifiedDeployment
    ,   state_into_prefix = ""
    ,   state_outof_prefix = ""
    }

pop :: SparkCompiler Declaration
pop = do
    dec <- gets state_declarations_left
    modify (\s -> s {state_declarations_left = tail dec})
    return $ head dec

done :: SparkCompiler Bool
done = fmap null $ gets state_declarations_left

compileDeployments :: SparkCompiler ()
compileDeployments = do
    d <- done
    if d
    then return ()
    else processDeclaration >> compileDeployments

add :: Deployment -> SparkCompiler ()
add dep = tell [dep]

processDeclaration :: SparkCompiler ()
processDeclaration = do
    dec <- pop
    case dec of
        Deploy src dst kind -> do
            dir <- gets state_current_directory
            override <- gets state_deployment_kind_override
            let resultKind = case (override, kind) of
                    (UnspecifiedDeployment, UnspecifiedDeployment) -> LinkDeployment
                    (o, UnspecifiedDeployment) -> o
                    (_, k) -> k
            outof <- gets state_outof_prefix
            into <- gets state_into_prefix
            let source = dir </> outof </> src
            let destination = into </> dst
            let dep = case resultKind of
                    LinkDeployment -> Link source destination
                    CopyDeployment -> Copy source destination
                    UnspecifiedDeployment -> error "report to author if you see this message"
            add dep
        SparkOff st -> do
            case st of
                TargetGit repo -> error "not yet implemented"
                TargetCardName name -> do
                    allCards <- gets state_all_cards
                    case find (\(Card (Just n) _ _) -> n == name) allCards of -- FIXME this is unsafe
                        Nothing -> error "card not found" -- FIXME this is unsafe as well.
                        Just c@(Card cn fp dcs) -> do
                            before <- get
                            modify (\s -> s {state_declarations_left = dcs
                                            ,state_current_card = c})
                            compileDeployments
                            put before



        IntoDir dir -> modify (\s -> s {state_into_prefix = dir} )
        OutofDir dir -> modify (\s -> s {state_outof_prefix = dir} )
        DeployKindOverride kind -> modify (\s -> s {state_deployment_kind_override = kind })
        Block ds -> do
            before <- get
            modify (\s -> s {state_declarations_left = ds})
            compileDeployments
            put before
