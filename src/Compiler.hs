{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import qualified Flags
import qualified Frontend             as Syn

import qualified Data.Text.Lazy       as L

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State

-- Compiler Monad

type CompilerMonad =
  ExceptT Msg (StateT CompilerState IO)

newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFix
    , MonadPlus
    , MonadIO
    , MonadState CompilerState
    , MonadError Msg)


-- Compiler State

data CompilerState = CompilerState
  { _fname   :: Maybe FilePath   -- File path
  , _imports :: [FilePath]       -- Loaded modules
  , _src     :: Maybe L.Text     -- File source
  , _ast     :: Maybe Syn.Module -- Frontend AST
  , _flags   :: Flags.Flags      -- Compiler flags
  }

emptyCS :: CompilerState
emptyCS = CompilerState
  { _fname = Nothing
  , _imports = mempty
  , _src = Nothing
  , _ast = Nothing
  , _flags = Flags.emptyFlags
  }


-- Types

type Pos = String

type Msg = String

-- Run the compiler pipeline
runCompilerM
  :: CompilerM a -> CompilerState -> IO (Either Msg a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

-- Life IO action into the Compiler IO layer
inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

-- Conditional execute mondic action if a flag is set
ifSet :: (Flags.Flags -> Bool) -> CompilerM a -> CompilerM ()
ifSet f m = do
  flags <- gets _flags
  when (f flags) (void m)

