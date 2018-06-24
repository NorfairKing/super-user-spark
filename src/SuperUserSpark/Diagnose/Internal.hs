module SuperUserSpark.Diagnose.Internal where

import Import

import qualified Data.ByteString as SB
import Data.Hashable
import System.Posix.Files
       (getSymbolicLinkStatus, isBlockDevice, isCharacterDevice,
        isDirectory, isNamedPipe, isRegularFile, isSocket, isSymbolicLink,
        readSymbolicLink)

import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.Diagnose.Types

diagnoseDeployment :: BakedDeployment -> IO DiagnosedDeployment
diagnoseDeployment (Deployment bds kind) = do
    ddirs <- diagnoseDirs bds
    return $ Deployment ddirs kind

diagnoseDirs :: DeploymentDirections AbsP
             -> IO (DeploymentDirections DiagnosedFp)
diagnoseDirs (Directions srcs dst) =
    Directions <$> mapM diagnoseAbsP srcs <*> diagnoseAbsP dst

diagnoseAbsP :: AbsP -> IO DiagnosedFp
diagnoseAbsP fp = do
    d <- diagnoseFp fp
    hash_ <- hashFilePath fp
    return $ D fp d hash_

diagnoseFp :: AbsP -> IO Diagnostics
diagnoseFp absp = do
    let fp = toPath absp
    ms <- forgivingAbsence $ getSymbolicLinkStatus fp
    case ms of
        Nothing -> pure Nonexistent
        Just s | isBlockDevice s || isCharacterDevice s || isSocket s || isNamedPipe s
                    -> return IsWeird
               | isSymbolicLink s -> do
                        point <- readSymbolicLink fp
                        -- TODO check what happens with relative links.
                        apoint <- AbsP <$> parseAbsFile point
                        return $ IsLinkTo apoint
               | otherwise -> pure $
                         if isDirectory s
                             then IsDirectory
                             else if isRegularFile s
                                      then IsFile
                                      else IsWeird

-- | Hash a filepath so that two filepaths with the same contents have the same hash
hashFilePath :: AbsP -> IO HashDigest
hashFilePath fp = do
    d <- diagnoseFp fp
    case d of
        IsFile -> hashFile fp
        IsDirectory -> hashDirectory fp
        IsLinkTo _ -> return $ HashDigest $ hash ()
        IsWeird -> return $ HashDigest $ hash ()
        Nonexistent -> return $ HashDigest $ hash ()

hashFile :: AbsP -> IO HashDigest
hashFile fp = HashDigest . hash <$> SB.readFile (toPath fp)

hashDirectory :: AbsP -> IO HashDigest
hashDirectory fp = do
    tdir <- parseAbsDir (toPath fp)
    walkDirAccum Nothing writer_ tdir
  where
    writer_ _ _ files = do
        hashes <- mapM (hashFile . AbsP) files
        pure $ HashDigest $ hash hashes
