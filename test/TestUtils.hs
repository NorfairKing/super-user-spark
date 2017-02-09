module TestUtils
    ( module TestUtils
    , module Debug.Trace
    ) where

import Control.Monad (filterM, forM_, when)
import Debug.Trace
import System.Directory
       (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath.Posix ((</>))
import Test.Hspec

concerningContents :: (FilePath -> String -> SpecWith a)
                   -> (FilePath -> SpecWith a)
concerningContents func file = (runIO $ readFile file) >>= func file

forFileInDirss :: [FilePath] -> (FilePath -> SpecWith a) -> SpecWith a
forFileInDirss [] _ = return ()
forFileInDirss dirs func =
    forM_ dirs $ \dir -> do
        exists <- runIO $ doesDirectoryExist dir
        when exists $ do
            files <- runIO $ getDirectoryContents dir
            let ffiles = filter (`notElem` [".", ".."]) files
            let fullFiles = map (dir </>) ffiles
            fs <- runIO $ filterM doesFileExist fullFiles
            forM_ fs func
            recdirs <- runIO $ filterM doesDirectoryExist fullFiles
            forFileInDirss recdirs func

pend :: SpecWith ()
pend = it "is still missing some tests" pending
