module Git where

import           System.Process (createProcess, shell)

import           Types

type GitRepoController = StateT GitRepoControllerState Sparker
data GitRepoControllerState = GitRepoControllerState {
        state_repo :: GitRepo
    }

runGitRepoController :: GitRepo -> GitRepoController a -> Sparker a
runGitRepoController repo func = do
    (a,_) <- runStateT (ensureRepoConsistency >> func) initialState
    return a
  where
    initialState :: GitRepoControllerState
    initialState = GitRepoControllerState {
            state_repo = repo
        }

ensureRepoConsistency :: GitRepoController ()
ensureRepoConsistency = do
    b <- isCloned
    if b
    then return () -- TODO, is there some way to check the integrity of the clone?
    else clone


isCloned :: GitRepoController Bool
isCloned = undefined

clone :: GitRepoController ()
clone = do
    repo <- gets state_repo
    let proc = shell $ "git clone " ++ show repo
    liftIO $ createProcess proc -- TODO wait for completion
    return ()

