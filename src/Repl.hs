{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl
  ( entry
  ) where

import           Flags
import           Monad
import Pretty
import Compiler

import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.IO      as L

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Data.List              (isPrefixOf)

import           System.Console.Repline
import           System.Exit


-- Types

data IState = IState
  { _compilerState :: CompilerState
  }

initState :: CompilerState -> IState
initState = IState

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-- Execution

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  cs <- gets _compilerState
  let cs' = cs { _src = Just source }

  (cm, cs'') <- liftIO $ runCompilerM compileLine cs'
  hoistErr cm

  when update (modify (\st -> st { _compilerState = cs''}))

  return ()

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

-- Commands

showMsg :: String -> Repl ()
showMsg s = liftIO $ L.putStrLn $ L.pack s

-- Eventually change colour to red
showError :: String -> Repl ()
showError s = liftIO $ L.putStrLn $ L.pack s

changeFlag :: [String] -> String -> Bool -> Repl ()
changeFlag [flag] name change = do
  cs <- gets _compilerState
  let flags = _flags cs
  case setFlag flag change flags of
    Just flags' -> do
      let cs' = cs { _flags = flags' }
      modify (\st -> st { _compilerState = cs' })
    Nothing -> showError $ "Flag " ++ flag ++ " is invalid"
changeFlag _ name _ = showError $ name ++ " command requires flag name as argument"

set :: [String] -> Repl ()
set flags = changeFlag flags "set" True

unset :: [String] -> Repl ()
unset flags = changeFlag flags "unset" False

flags :: a -> Repl ()
flags _ = do
  cs <- gets _compilerState
  showMsg $ show $ _flags cs

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Interactive Shell

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [(":load", fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":set", ":unset", ":flags", ":quit"]
  return $ filter (isPrefixOf n) cmds

options :: [(String, [String] -> Repl ())]
options =
  [ ("set", set)
  , ("unset", unset)
  , ("flags", flags)
  , ("quit", quit)
  ]

-- Entry Point

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: CompilerState -> Repl a -> IO ()
shell cs pre = flip evalStateT (initState cs)
  $ evalRepl "styx> " cmd options completer pre

entry :: CompilerState -> IO ()
entry cs = do
  L.putStrLn $ L.pack $ banner "Hello"
  shell cs (return ())

