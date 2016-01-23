{-# LANGUAGE OverloadedStrings #-}
module Types
    (
      module Types
    , module CoreTypes
    , module Monad
    , module Config.Types

    , module Control.Monad.Except
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Writer
    , module Control.Monad.Trans
    , module Text.Parsec
    ) where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.State    (StateT, get, gets, modify, put,
                                         runStateT)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)

import           Text.Parsec            (ParseError)

import           Config.Types
import           CoreTypes
import           Monad
import           Parser.Types

---[ Card References ]--
type CompilerCardReference = CardFileReference

type CompiledCardReference = FilePath

instance Read CardFileReference where
    readsPrec _ fp = case length (words fp) of
                      1 -> [(CardFileReference fp Nothing ,"")]
                      2 -> let [f, c] = words fp
                            in [(CardFileReference f (Just $ CardNameReference c), "")]
                      _ -> []

---[ Options ]---

data GlobalOptions = GlobalOptions {
      opt_lineUp              :: Bool
    , opt_indent              :: Int
    , opt_trailingNewline     :: Bool
    , opt_alwaysQuote         :: Bool
    , opt_compress            :: Bool
    , opt_output              :: Maybe FilePath
    , opt_format              :: CompileFormat
    , opt_kind                :: Maybe DeploymentKind
    , opt_overrride           :: Maybe DeploymentKind
    , opt_thoroughness        :: CheckThoroughness
    , opt_replace_links       :: Bool
    , opt_replace_files       :: Bool
    , opt_replace_directories :: Bool
    , opt_replace             :: Bool
    , opt_debug               :: Bool
    } deriving (Show, Eq)

---[ Pretty Types ]---

type SparkFormatter = StateT FormatterState (WriterT String Sparker)
data FormatterState = FormatterState {
        state_current_indent        :: Int
    ,   state_longest_src           :: Int
    ,   state_newline_before_deploy :: Bool
    }
    deriving (Show, Eq)

runSparkFormatter :: FormatterState -> SparkFormatter a -> Sparker ((a, FormatterState), String)
runSparkFormatter state func = runWriterT (runStateT func state)


