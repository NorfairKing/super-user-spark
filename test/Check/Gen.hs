{-# OPTIONS_GHC -Wno-orphans #-}

module Check.Gen where

import Check.Types
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5
import Language.Gen ()
import Test.QuickCheck

instance Arbitrary CheckResult where
    arbitrary =
        oneof
            [ pure AlreadyDone
            , Ready <$> arbitrary
            , Dirty <$> arbitrary <*> arbitrary <*> arbitrary
            , Impossible <$> arbitrary
            ]

instance Arbitrary Instruction where
    arbitrary = Instruction <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CleanupInstruction where
    arbitrary =
        oneof
            [ CleanFile <$> arbitrary
            , CleanDirectory <$> arbitrary
            , CleanLink <$> arbitrary
            ]

instance Arbitrary DiagnosedDeployment where
    arbitrary = Diagnosed <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Diagnostics where
    arbitrary =
        oneof
            [ pure Nonexistent
            , pure IsFile
            , pure IsDirectory
            , IsLinkTo <$> arbitrary
            , pure IsWeird
            ]

instance Arbitrary DiagnosedFp where
    arbitrary = D <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MD5Digest where
    arbitrary = md5 <$> arbitrary

instance Arbitrary LB.ByteString where
    arbitrary = LB.pack <$> arbitrary
