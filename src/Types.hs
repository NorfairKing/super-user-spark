{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    , module Control.Monad.Supply
    , module Control.Monad.Writer
    , module Control.Monad.Trans
    , module Control.Monad.Identity
    , module Text.Parsec

    , module Debug.Trace
    ) where

import           Control.Monad.Except   (ExceptT, mapExceptT, runExceptT,
                                         throwError, withExceptT)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.IO.Class (MonadIO (..), liftIO)
import           Control.Monad.Reader   (MonadReader (..), ReaderT, ask, asks,
                                         mapReaderT, runReaderT)
import           Control.Monad.State    (MonadState (..), StateT, evalStateT,
                                         get, gets, modify, put, runStateT)
import           Control.Monad.Supply   (MonadSupply (..), SupplyT, evalSupplyT)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Writer   (MonadWriter (..), WriterT, execWriterT,
                                         runWriterT, tell)
import           Debug.Trace

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

-- Extra instances
instance MonadWriter w m => MonadWriter w (SupplyT d m) where
    writer = lift . writer
    tell = lift . tell
    listen = error "don't listen!"
    pass = error "don't pass!"

instance MonadState s m => MonadState s (SupplyT d m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadReader c m => MonadReader c (SupplyT d m) where
    ask = lift ask
    local = error "don't local!"
    reader = lift . reader

