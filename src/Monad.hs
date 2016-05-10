module Monad where

import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Text.Parsec          (ParseError)

import           Config.Types

-- | The base monad
type Sparker = ExceptT SparkError (ReaderT SparkConfig IO)

runSparker :: SparkConfig -> Sparker a -> IO (Either SparkError a)
runSparker conf func = runReaderT (runExceptT func) conf

data SparkError = ParseError ParseError
                | PreCompileError [PreCompileError]
                | CompileError CompileError
                | DeployError DeployError
                | UnpredictedError String
    deriving (Show, Eq)

type CompileError = String
type PreCompileError = String
type DeployError = String

