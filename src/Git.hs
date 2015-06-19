module Git where

import           System.Directory (doesDirectoryExist, getCurrentDirectory,
                                   getDirectoryContents, setCurrentDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (takeDirectory, takeFileName, (</>))
import           System.Process   (createProcess, shell, waitForProcess)

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
    then do
        -- TODO, is there some way to check the integrity of the clone?
        -- Set repo root var
        return ()
    else clone


isCloned :: GitRepoController Bool
isCloned = do
    -- FIXME This is very basic ...
    root <- repoRoot
    liftIO $ doesDirectoryExist root


clone :: GitRepoController ()
clone = do
    repo <- gets state_repo
    let proc = shell $ "git clone " ++ show repo
    (_,_,_,ph) <- liftIO $ createProcess proc
    ec <- liftIO $ waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> throwGitError "Cloning Failed."
    return ()

checkout :: Branch -> GitRepoController ()
checkout branch = do
    cd <- liftIO $ getCurrentDirectory
    let proc = shell $ "git checkout " ++ branch
    (_,_,_,ph) <- liftIO $ createProcess proc
    ec <- liftIO $ waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure _ -> throwGitError "Checkout Failed."
    liftIO $ setCurrentDirectory cd



throwGitError :: String -> GitRepoController ()
throwGitError str = do
    repo <- gets state_repo
    throwError $ GitError $ GitRepoError repo str



readGitFile :: FilePath -> GitRepoController String
readGitFile fp = do
    rfp <- repoFilePath fp
    liftIO $ readFile rfp

repoFilePath :: FilePath -> GitRepoController FilePath
repoFilePath fp = do
    root <- repoRoot
    return $ root </> fp

repoRoot :: GitRepoController FilePath
repoRoot = do
    repo <- gets state_repo
    return $ takeFileName $ repo_path repo

listRepoDir :: FilePath -> GitRepoController [FilePath]
listRepoDir fp = do
    rfp <- repoFilePath fp
    dc <- liftIO $ getDirectoryContents rfp
    root <- repoRoot
    return $ fmap (root </>) dc


