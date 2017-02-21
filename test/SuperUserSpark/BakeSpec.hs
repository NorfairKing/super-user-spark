{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module SuperUserSpark.BakeSpec where

import TestImport

import Data.Either (isLeft)
import Data.Maybe (isNothing)

import qualified System.FilePath as FP
import System.Posix.Files (createSymbolicLink)

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Compiler.Types
import SuperUserSpark.OptParse.Gen ()
import SuperUserSpark.Parser.Gen

spec :: Spec
spec = do
    instanceSpec
    bakeSpec

instanceSpec :: Spec
instanceSpec =
    parallel $ do
        eqSpec @BakeAssignment
        genValidSpec @BakeAssignment
        eqSpec @BakeCardReference
        genValidSpec @BakeCardReference
        eqSpec @BakeSettings
        genValidSpec @BakeSettings
        eqSpec @BakeError
        genValidSpec @BakeError
        eqSpec @BakedDeployment
        genValidSpec @BakedDeployment
        jsonSpecOnValid @BakedDeployment
        eqSpec @AbsP
        genValidSpec @AbsP
        eqSpec @(DeploymentDirections AbsP)
        genValidSpec @(DeploymentDirections AbsP)
        jsonSpecOnValid @(DeploymentDirections AbsP)
        eqSpec @ID
        genValidSpec @ID

bakeSpec :: Spec
bakeSpec =
    parallel $ do
        describe "bakeFilePath" $ do
            it "works for these unit test cases without variables" $ do
                let b root fp s = do
                        absp <- AbsP <$> parseAbsFile s
                        rp <- parseAbsDir root
                        runReaderT
                            (runExceptT (bakeFilePath fp))
                            defaultBakeSettings {bakeRoot = rp} `shouldReturn`
                            Right absp
                b "/home/user/hello" "a/b/c" "/home/user/hello/a/b/c"
                b "/home/user/hello" "/home/user/.files/c" "/home/user/.files/c"
            it "works for a simple home-only variable situation" $ do
                forAll genValid $ \root -> do
                    let b home fp s = do
                            absp <- AbsP <$> parseAbsFile s
                            runReaderT
                                (runExceptT (bakeFilePath fp))
                                defaultBakeSettings
                                { bakeRoot = root
                                , bakeEnvironment = [("HOME", home)]
                                } `shouldReturn`
                                Right absp
                    b "/home/user" "~/a/b/c" "/home/user/a/b/c"
                    b "/home" "~/c" "/home/c"
            here <- runIO getCurrentDir
            let sandbox = here </> $(mkRelDir "test_sandbox")
            before_ (ensureDir sandbox) $
                after_ (removeDirRecur sandbox) $
                it
                    "does not follow toplevel links when the completed paths are relative" $ do
                    let file = $(mkRelFile "file")
                    let from = $(mkRelDir "from") </> file
                    let to = $(mkRelDir "to") </> file
                    withCurrentDir sandbox $ do
                        ensureDir $ parent $ sandbox </> from
                        writeFile (sandbox </> from) "contents"
                        ensureDir $ parent $ sandbox </> to
                        createSymbolicLink
                            (toFilePath $ sandbox </> from)
                            (toFilePath $ sandbox </> to)
                    let runBake f =
                            runReaderT
                                (runExceptT f)
                                (defaultBakeSettings
                                 {bakeRoot = sandbox, bakeEnvironment = []})
                    runBake (bakeFilePath (toFilePath to)) `shouldReturn`
                        (Right $ AbsP $ sandbox </> to)
                    runBake (bakeFilePath (toFilePath from)) `shouldReturn`
                        (Right $ AbsP $ sandbox </> from)
            it "follows directory-level links" $ pending
        describe "defaultBakeSettings" $
            it "is valid" $ isValid defaultBakeSettings
        describe "formatBakeError" $ do
            it "only ever produces valid strings" $
                producesValid formatBakeError
        describe "complete" $ do
            it "only ever produces a valid filepath" $ validIfSucceeds2 complete
            it
                "replaces the home directory as specified for simple home directories and simple paths" $ do
                forAll arbitrary $ \env ->
                    forAll generateWord $ \home ->
                        forAll generateWord $ \fp ->
                            complete (("HOME", home) : env) ("~" FP.</> fp) `shouldBe`
                            Right (home FP.</> fp)
        describe "parseId" $ do
            it "only ever produces valid IDs" $ producesValid parseId
            it "Figures out the home directory in these cases" $ do
                parseId "~" `shouldBe` [Var "HOME"]
                parseId "~/ab" `shouldBe` [Var "HOME", Plain "/ab"]
            it "Works for these cases" $ do
                parseId "" `shouldBe` []
                parseId "file" `shouldBe` [Plain "file"]
                parseId "something$(with)variable" `shouldBe`
                    [Plain "something", Var "with", Plain "variable"]
                parseId "$(one)$(two)$(three)" `shouldBe`
                    [Var "one", Var "two", Var "three"]
        describe "replaceId" $ do
            it "only ever produces valid FilePaths" $ validIfSucceeds2 replaceId
            it "leaves plain ID's unchanged in any environment" $
                forAll arbitrary $ \env ->
                    forAll arbitrary $ \s ->
                        replaceId env (Plain s) `shouldBe` Right s
            it "returns Left if a variable is not in the environment" $
                forAll arbitrary $ \var ->
                    forAll (arbitrary `suchThat` (isNothing . lookup var)) $ \env ->
                        replaceId env (Var var) `shouldSatisfy` isLeft
            it "replaces a variable if it's in the environment" $
                forAll arbitrary $ \var ->
                    forAll arbitrary $ \val ->
                        forAll (arbitrary `suchThat` (isNothing . lookup var)) $ \env1 ->
                            forAll
                                (arbitrary `suchThat` (isNothing . lookup var)) $ \env2 ->
                                replaceId
                                    (env1 ++ [(var, val)] ++ env2)
                                    (Var var) `shouldBe`
                                Right val
