module SuperUserSpark
    ( spark
    ) where

import Import

import SuperUserSpark.Bake
import SuperUserSpark.Check
import SuperUserSpark.Compiler
import SuperUserSpark.Deployer
import SuperUserSpark.Diagnose
import SuperUserSpark.OptParse
import SuperUserSpark.Parser

spark :: IO ()
spark = getDispatch >>= dispatch

dispatch :: Dispatch -> IO ()
dispatch (DispatchParse pas) = parseFromArgs pas
dispatch (DispatchCompile cas) = compileFromArgs cas
dispatch (DispatchBake bas) = bakeFromArgs bas
dispatch (DispatchDiagnose bas) = diagnoseFromArgs bas
dispatch (DispatchCheck cas) = checkFromArgs cas
dispatch (DispatchDeploy das) = deployFromArgs das
