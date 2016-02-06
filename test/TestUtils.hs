module TestUtils (
      module TestUtils
    , module Debug.Trace
    ) where

import           Test.Hspec
import           Test.Hspec.Core.Spec
import           Test.QuickCheck

import           Debug.Trace

import           Control.Monad         (filterM, forM, forM_, when)

import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getDirectoryContents)
import           System.FilePath.Posix ((<.>), (</>))

concerningContents :: (FilePath -> String -> SpecWith a) -> (FilePath -> SpecWith a)
concerningContents func file = (runIO $ readFile file) >>= func file

forFileInDirss :: [FilePath] -> (FilePath -> SpecWith a) -> SpecWith a
forFileInDirss [] _ = return ()
forFileInDirss dirs func = forM_ dirs $ \dir -> do
    exists <- runIO $ doesDirectoryExist dir
    when exists $ do
        files <- runIO $ getDirectoryContents dir
        let ffiles = filter (`notElem` [".", ".."]) files
        let fullFiles = map (dir </>) ffiles
        fs <- runIO $ filterM doesFileExist fullFiles
        forM_ fs func
        recdirs <- runIO $ filterM doesDirectoryExist fullFiles
        forFileInDirss recdirs func

pend = it "is still missing some tests" pending

