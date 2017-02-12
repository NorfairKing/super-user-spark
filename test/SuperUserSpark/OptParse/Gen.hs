{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.OptParse.Gen where

import TestImport

import SuperUserSpark.OptParse.Types

instance GenUnchecked Dispatch

instance GenValid Dispatch

instance Arbitrary Dispatch where
    arbitrary = genValid

instance GenUnchecked ParseArgs

instance GenValid ParseArgs

instance Arbitrary ParseArgs where
    arbitrary = genValid

instance GenUnchecked CompileArgs

instance GenValid CompileArgs

instance Arbitrary CompileArgs where
    arbitrary = genValid

instance GenUnchecked CompileFlags

instance GenValid CompileFlags

instance Arbitrary CompileFlags where
    arbitrary = genValid

instance GenUnchecked BakeArgs

instance GenValid BakeArgs

instance Arbitrary BakeArgs where
    arbitrary = genValid

instance GenUnchecked BakeFlags

instance GenValid BakeFlags

instance Arbitrary BakeFlags where
    arbitrary = genValid

instance GenUnchecked CheckArgs

instance GenValid CheckArgs

instance Arbitrary CheckArgs where
    arbitrary = genValid

instance GenUnchecked CheckFlags

instance GenValid CheckFlags

instance Arbitrary CheckFlags where
    arbitrary = genValid

instance GenUnchecked DeployArgs

instance GenValid DeployArgs

instance Arbitrary DeployArgs where
    arbitrary = genValid

instance GenUnchecked DeployFlags

instance GenValid DeployFlags

instance Arbitrary DeployFlags where
    arbitrary = genValid
