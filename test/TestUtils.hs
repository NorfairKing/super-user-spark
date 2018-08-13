{-# LANGUAGE TemplateHaskell #-}

module TestUtils where

import TestImport

shouldParseDir :: Path Rel Dir
shouldParseDir = $(mkRelDir "shouldParseDir")

shouldNotParseDir :: Path Rel Dir
shouldNotParseDir = $(mkRelDir "shouldNotParseDir")

shouldCompileDir :: Path Rel Dir
shouldCompileDir = $(mkRelDir "shouldCompileDir")

shouldNotCompileDir :: Path Rel Dir
shouldNotCompileDir = $(mkRelDir "shouldNotCompileDir")

concerningContents :: (Path Abs File -> String -> SpecWith a)
                   -> (Path Abs File -> SpecWith a)
concerningContents func file = (runIO . readFile $ toFilePath file) >>= func file

forFileInDirss :: [Path Abs Dir] -> (Path Abs File -> SpecWith a) -> SpecWith a
forFileInDirss [] _ = return ()
forFileInDirss dirs func =
    forM_ dirs $ \dir -> do
        exists <- runIO $ doesDirExist dir
        when exists $ do
            files <- runIO $ snd <$> listDirRecur dir
            forM_ files func

pend :: SpecWith ()
pend = it "is still missing some tests" pending
