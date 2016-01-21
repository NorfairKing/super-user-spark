module TestUtils where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad         (forM, forM_)

import           System.Directory      (getDirectoryContents)
import           System.FilePath.Posix ((<.>), (</>))

concerningContents :: (FilePath -> String -> SpecWith a) -> (FilePath -> SpecWith a)
concerningContents func file = runIO (readFile file) >>= func file

forFileInDirss :: [FilePath] -> (FilePath -> SpecWith a) -> SpecWith a
forFileInDirss dirs func = do
    allFiles <- fmap concat $ forM dirs $ \dir -> do
        files <- runIO (getDirectoryContents dir)
        let ffiles = filter (`notElem` [".", ".."]) files
        return $ map (dir </>) ffiles
    forM_ allFiles func

pend = it "has no tests yet" pending

