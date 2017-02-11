module SuperUserSpark.SeedSpec where

import TestImport

import SuperUserSpark.Compiler.Gen ()
import SuperUserSpark.Compiler.TestUtils
import SuperUserSpark.Compiler.Types
import SuperUserSpark.PreCompiler
import SuperUserSpark.Seed
import System.FilePath.Posix (isAbsolute)
import TestUtils

spec :: Spec
spec = parallel $ do pureSeedSpec

pureSeedSpec :: Spec
pureSeedSpec =
    describe "seed" $ do
        it "ensures that sources are absolute if the seed is an absolute path" $ do
            once $
                forAll
                    (arbitrary `suchThat` cleanBy cleanFilePath `suchThat`
                     isAbsolute) $ \fp ->
                    forAll arbitrary $ \ds ->
                        all (\d -> all (isAbsolute) $ deploymentSources d) $
                        seed fp ds
        pend
