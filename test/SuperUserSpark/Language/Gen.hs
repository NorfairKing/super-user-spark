{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.Language.Gen where

import TestImport

import SuperUserSpark.CoreTypes
import SuperUserSpark.Language.Types

instance GenUnchecked SparkFile where
    genUnchecked = SparkFile <$> genUnchecked <*> genUnchecked

instance GenValid SparkFile

instance GenUnchecked Card where
    genUnchecked = Card <$> genUnchecked <*> genUnchecked

instance GenValid Card

instance GenUnchecked Declaration where
    genUnchecked = resize 5 $ sized go
      where
        go 0 =
            oneof
                [ SparkOff <$> genUnchecked
                , Deploy <$> genUnchecked <*> genUnchecked <*> genUnchecked
                , IntoDir <$> genUnchecked
                , OutofDir <$> genUnchecked
                , DeployKindOverride <$> genUnchecked
                , Alternatives <$> genUnchecked
                , Block <$> genUnchecked
                ]
        go n =
            oneof
                [ SparkOff <$> genUnchecked
                , Deploy <$> genUnchecked <*> genUnchecked <*> genUnchecked
                , IntoDir <$> genUnchecked
                , OutofDir <$> genUnchecked
                , DeployKindOverride <$> genUnchecked
                , Alternatives <$> genUnchecked
                , Block <$> listOf (go $ n - 1)
                ]

instance GenValid Declaration

instance GenUnchecked DeploymentKind where
    genUnchecked = elements [LinkDeployment, CopyDeployment]

instance GenValid DeploymentKind

instance GenUnchecked CardNameReference where
    genUnchecked = CardNameReference <$> genUnchecked

instance GenValid CardNameReference

instance GenUnchecked CardFileReference where
    genUnchecked = CardFileReference <$> genUnchecked <*> genUnchecked

instance GenValid CardFileReference

instance GenUnchecked CardReference where
    genUnchecked = oneof [CardFile <$> genUnchecked, CardName <$> genUnchecked]

instance GenValid CardReference
