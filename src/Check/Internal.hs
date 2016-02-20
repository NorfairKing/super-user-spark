module Check.Internal where

import           Check.Types
import           Control.Monad              (mapM)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.Digest.Pure.MD5
import           System.Directory           (getDirectoryContents)
import           System.Exit                (ExitCode (..))
import           System.FilePath            (dropFileName, normalise, (</>))
import           System.Posix.Files         (createSymbolicLink, fileExist,
                                             getSymbolicLinkStatus,
                                             isBlockDevice, isCharacterDevice,
                                             isDirectory, isNamedPipe,
                                             isRegularFile, isSocket,
                                             isSymbolicLink, readSymbolicLink)
import           System.Process             (system)


hashFilePath :: FilePath -> IO HashDigest
hashFilePath fp = do
    d <- diagnoseFp fp
    case d of
        IsFile -> hashFile fp
        _ -> undefined

hashFile :: FilePath -> IO HashDigest
hashFile fp = md5 <$> LB.readFile fp

hashDirectory :: FilePath -> IO HashDigest
hashDirectory fp = do
    rawContents <- getDirectoryContents fp
    let contents = map (fp </>) . filter (\f -> not $ f == "." || f == "..") $ rawContents
    hashes <- mapM hashFilePath contents
    let hashbs = map (LBC.pack . show) hashes
    return $ md5 $ LB.concat hashbs


diagnose :: FilePath -> IO DiagnosedFp
diagnose fp = do
    d <- diagnoseFp fp
    hash <- hashFilePath fp
    return $ D fp d hash

diagnoseFp :: FilePath -> IO Diagnostics
diagnoseFp fp = do
    e <- fileExist fp
    if e
    then do
        s <- getSymbolicLinkStatus fp
        if isBlockDevice s || isCharacterDevice s || isSocket s || isNamedPipe s
        then return IsWeird
        else do
            if isSymbolicLink s
            then do
                point <- readSymbolicLink fp
                return $ IsLinkTo point
            else if isDirectory s
                then return IsDirectory
                else if isRegularFile s
                    then return IsFile
                    else error $ "File " ++ fp ++ " was neither a block device, a character device, a socket, a named pipe, a symbolic link, a directory or a regular file"
    else do
        -- If a link exists, but it points to something that doesn't exist, it is considered as non-existent by `fileExist`
        es <- system $ unwords ["test", "-L", fp]
        case es of
            ExitSuccess -> do
                point <- readSymbolicLink fp
                return $ IsLinkTo point
            ExitFailure _ -> return Nonexistent

