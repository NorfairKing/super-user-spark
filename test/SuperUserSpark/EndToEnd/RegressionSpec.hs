{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module SuperUserSpark.EndToEnd.RegressionSpec
    ( spec
    ) where

import TestImport

import System.Directory
import System.Environment (withArgs)
import System.Exit (ExitCode(ExitFailure))
import System.Posix.Files

import SuperUserSpark
import SuperUserSpark.Utils

spec :: Spec
spec = linkThenCopySpec

linkThenCopySpec :: Spec
linkThenCopySpec = do
    here <- runIO getCurrentDir
    let sandbox = here </> $(mkRelDir "test_sandbox")
    let setup = ensureDir sandbox
    let teardown = removeDirRecur sandbox
    beforeAll_ setup $
        afterAll_ teardown $
        describe "link then copy regression" $ do
            let runSpark args = do
                    putStrLn . unwords $ "spark" : args
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
                    writeFile (toFilePath from) "contents"
                    let setUpCardFile cf = do
                            runSpark ["parse", toFilePath cf]
                            runSpark ["compile", toFilePath cf]
                            runSpark ["bake", toFilePath cf]
                            runSpark ["check", toFilePath cf]
                    writeFile (toFilePath from) "contents"
                        -- Set up the first card file
                    writeFile
                        (toFilePath cf)
                        "card link { kind link; into to; outof from; file }"
                    setUpCardFile cf
                    runSpark ["deploy", toFilePath cf]
                        -- Set up the second card file
                    writeFile
                        (toFilePath cf)
                        "card link { kind copy; into to; outof from; file }"
                    setUpCardFile cf
                    runSpark ["deploy", toFilePath cf] `shouldThrow`
                        (\case
                             ExitFailure _ -> True
                             _ -> False)
