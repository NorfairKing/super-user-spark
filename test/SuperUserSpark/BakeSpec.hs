{-# LANGUAGE TypeApplications #-}

module SuperUserSpark.BakeSpec where

import qualified Prelude as P (writeFile)
import TestImport hiding ((</>), removeFile, writeFile)

import Data.Either (isLeft)
import Data.Maybe (isNothing)

import System.Directory hiding (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.Posix.Files

import SuperUserSpark.Bake
import SuperUserSpark.Bake.Gen ()
import SuperUserSpark.Bake.Internal
import SuperUserSpark.Bake.Types
import SuperUserSpark.Check.Internal
import SuperUserSpark.Check.Types
import SuperUserSpark.Deployer
import SuperUserSpark.Deployer.Internal
import SuperUserSpark.Deployer.Types
import SuperUserSpark.OptParse.Gen ()
import SuperUserSpark.Parser.Gen
import SuperUserSpark.Utils

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
        eqSpec @AbsP
        genValidSpec @AbsP
        eqSpec @(DeploymentDirections AbsP AbsP)
        genValidSpec @(DeploymentDirections AbsP AbsP)
        eqSpec @ID
        genValidSpec @ID

bakeSpec :: Spec
bakeSpec =
    parallel $ do
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
                            complete (("HOME", home) : env) ("~" </> fp) `shouldBe`
                            Right (home </> fp)
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
