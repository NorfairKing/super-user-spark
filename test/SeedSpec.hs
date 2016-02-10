module SeedSpec where

import           Compiler.Gen          ()
import           Compiler.Internal
import           Compiler.TestUtils
import           Compiler.Types
import           Seed
import           System.FilePath.Posix (isAbsolute)
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils

spec :: Spec
spec = parallel $ do
    pureSeedSpec

pureSeedSpec :: Spec
pureSeedSpec = describe "seed" $ do
    it "ensures that sources are absolute if the seed is an absolute path" $ do
        forAll (arbitrary `suchThat` cleanBy cleanFilePath `suchThat` isAbsolute) $ \fp ->
            forAll arbitrary $ \ds ->
                all (\d -> all (isAbsolute) $ deployment_srcs d) $ seed fp ds

    pend
