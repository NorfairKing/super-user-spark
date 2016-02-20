module Check.Gen where

import           Check.Types
import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.MD5
import           Language.Gen         ()
import           Test.QuickCheck

instance Arbitrary Diagnostics where
    arbitrary = oneof
        [ return Nonexistent
        , return IsFile
        , return IsDirectory
        , IsLinkTo <$> arbitrary
        , return IsWeird
        ]

instance Arbitrary DiagnosedFp where
    arbitrary = D <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MD5Digest where
    arbitrary = md5 <$> arbitrary

instance Arbitrary LB.ByteString where
    arbitrary = LB.pack <$> arbitrary
