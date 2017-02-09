{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Gen where

import CoreTypes
import Language.Types
import Test.QuickCheck

instance Arbitrary SparkFile where
    arbitrary = SparkFile <$> arbitrary <*> arbitrary

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Declaration where
    arbitrary = resize 5 $ sized go
      where
        go 0 =
            oneof
                [ SparkOff <$> arbitrary
                , Deploy <$> arbitrary <*> arbitrary <*> arbitrary
                , IntoDir <$> arbitrary
                , OutofDir <$> arbitrary
                , DeployKindOverride <$> arbitrary
                , Alternatives <$> arbitrary
                , Block <$> arbitrary
                ]
        go n =
            oneof
                [ SparkOff <$> arbitrary
                , Deploy <$> arbitrary <*> arbitrary <*> arbitrary
                , IntoDir <$> arbitrary
                , OutofDir <$> arbitrary
                , DeployKindOverride <$> arbitrary
                , Alternatives <$> arbitrary
                , Block <$> listOf (go $ n - 1)
                ]

instance Arbitrary DeploymentKind where
    arbitrary = elements [LinkDeployment, CopyDeployment]

instance Arbitrary CardNameReference where
    arbitrary = CardNameReference <$> arbitrary

instance Arbitrary CardFileReference where
    arbitrary = CardFileReference <$> arbitrary <*> arbitrary

instance Arbitrary CardReference where
    arbitrary = oneof [CardFile <$> arbitrary, CardName <$> arbitrary]
