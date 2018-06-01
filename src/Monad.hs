{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad where

import qualified Flags
import qualified Frontend             as Syn

import qualified Data.Text            as T

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import           System.Directory

-- Compiler Monad

type CompilerMonad =
  ExceptT CompilerError (StateT CompilerState IO)

newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState CompilerState
    , MonadError CompilerError)


-- Compiler State

data CompilerState = CompilerState
  { _fname   :: Maybe FilePath   -- File path
  , _imports :: [FilePath]       -- Loaded modules
  , _src     :: Maybe T.Text     -- File source
  , _ast     :: Maybe Syn.Module -- Frontend AST
  , _flags   :: Flags.Flags      -- Compiler flags
  } deriving (Show)

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

data CompilerError
  = FileNotFound FilePath
  deriving (Eq, Show)

-- Run the compiler pipeline
runCompilerM
  :: CompilerM a -> CompilerState -> IO (Either CompilerError a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

-- Life IO action into the Compiler IO layer
inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

-- Conditional execute mondic action if a flag is set
ifSet :: (Flags.Flags -> Bool) -> CompilerM a -> CompilerM ()
ifSet f m = do
  flags <- gets _flags
  when (f flags) (void m)

