module SuperUserSpark
    ( spark
    ) where

import Import

import SuperUserSpark.Check
import SuperUserSpark.Compiler
import SuperUserSpark.Deployer
import SuperUserSpark.OptParse
import SuperUserSpark.Parser

spark :: IO ()
spark = getDispatch >>= dispatch

dispatch :: Dispatch -> IO ()
dispatch (DispatchParse pas) = parseFromArgs pas
dispatch (DispatchCompile cas) = compileFromArgs cas
dispatch (DispatchCheck cas) = checkFromArgs cas
dispatch (DispatchDeploy das) = deployFromArgs das
