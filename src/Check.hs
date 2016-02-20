module Check where

import           Language.Types
import           Monad
import           System.Directory   (Permissions (..))
import           System.Exit        (ExitCode (..))
import           System.FilePath    (dropFileName, normalise, (</>))
import           System.Posix.Files (createSymbolicLink, fileExist,
                                     getSymbolicLinkStatus, isBlockDevice,
                                     isCharacterDevice, isDirectory,
                                     isNamedPipe, isRegularFile, isSocket,
                                     isSymbolicLink, readSymbolicLink)
import           System.Process     (system)
import           Types



data Diagnostics
    = Nonexistent
    | IsFile
    | IsDirectory
    | IsLink
    | IsWeird
    deriving (Show, Eq)



diagnose :: FilePath -> IO Diagnostics
diagnose fp = do
    e <- liftIO $ fileExist fp
    if e
    then do
        s <- liftIO $ getSymbolicLinkStatus fp
        if isBlockDevice s || isCharacterDevice s || isSocket s || isNamedPipe s
        then return IsWeird
        else do
            if isSymbolicLink s
            then return IsLink
            else if isDirectory s
                then return IsDirectory
                else if isRegularFile s
                    then return IsFile
                    else error $ "File " ++ fp ++ " was neither a block device, a character device, a socket, a named pipe, a symbolic link, a directory or a regular file"
    else do
        -- If a link exists, but it points to something that doesn't exist, it is considered as non-existent by `fileExist`
        es <- liftIO $ system $ unwords ["test", "-L", fp]
        case es of
            ExitSuccess -> return IsLink
            ExitFailure _ -> return Nonexistent



