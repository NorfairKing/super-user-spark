{-# OPTIONS_GHC -Wno-orphans #-}

module SuperUserSpark.OptParse.Gen where

import TestImport

import SuperUserSpark.OptParse.Types

instance GenUnchecked Dispatch

instance GenValid Dispatch

instance GenUnchecked ParseArgs

instance GenValid ParseArgs

instance GenUnchecked CompileArgs

instance GenValid CompileArgs

instance GenUnchecked CompileFlags

instance GenValid CompileFlags

instance GenUnchecked BakeArgs

instance GenValid BakeArgs

instance GenUnchecked BakeFlags

instance GenValid BakeFlags

instance GenUnchecked CheckArgs

instance GenValid CheckArgs

instance GenUnchecked CheckFlags

instance GenValid CheckFlags

instance GenUnchecked DiagnoseArgs

instance GenValid DiagnoseArgs

instance GenUnchecked DiagnoseFlags

instance GenValid DiagnoseFlags

instance GenUnchecked DeployArgs

instance GenValid DeployArgs

instance GenUnchecked DeployFlags

instance GenValid DeployFlags
