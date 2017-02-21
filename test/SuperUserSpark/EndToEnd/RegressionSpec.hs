{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.EndToEnd.RegressionSpec
    ( spec
    ) where

import TestImport

import System.Directory
import System.Environment (withArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Files

import SuperUserSpark
import SuperUserSpark.Utils

spec :: Spec
spec = do
    linkThenCopySpec

linkThenCopySpec :: Spec
linkThenCopySpec = do
    here <- runIO getCurrentDir
    let sandbox = here </> $(mkRelDir "test_sandbox")
    let setup = ensureDir sandbox
    let teardown = removeDirRecur sandbox
    beforeAll_ setup $
        afterAll_ teardown $ do
            describe "link then copy regression" $ do
                let runSpark args = do
                        putStrLn $ unwords $ "spark" : args
                        withArgs args spark
                it
                    "ensures that deploy fails when there is already a link that points to the file that is being copied." $
                    withCurrentDir sandbox $ do
                        let cf = sandbox </> $(mkRelFile "cardfile.sus")
                        let file = $(mkRelFile "file")
                        let from = sandbox </> $(mkRelDir "from") </> file
                        let to = sandbox </> $(mkRelDir "to") </> file
                        -- Set up the file
                        ensureDir (parent from)
                        writeFile from "contents"
                        -- Set up the first card file
                        writeFile
                            cf
                            "card link { kind link; into to; outof from; file }"
                        runSpark ["parse", toFilePath cf]
                        runSpark ["compile", toFilePath cf]
                        runSpark ["bake", toFilePath cf]
                        runSpark ["check", toFilePath cf]
                        runSpark ["deploy", toFilePath cf]
                        -- Set up the second card file
                        writeFile
                            cf
                            "card link { kind copy; into to; outof from; file }"
                        runSpark ["parse", toFilePath cf]
                        runSpark ["compile", toFilePath cf]
                        runSpark ["bake", toFilePath cf]
                        runSpark ["check", toFilePath cf]
                        runSpark ["deploy", toFilePath cf] `shouldThrow`
                            (\e ->
                                 case e of
                                     ExitSuccess -> True
                                     _ -> False)
