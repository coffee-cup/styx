{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Repl
  ( entry
  ) where

import           Compiler
import           Flags
import           Monad
import           Pretty

import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Data.List                  (isPrefixOf)

import           System.Console.Repline
import           System.Exit


-- Types

data IState = IState
  { _compilerState :: CompilerState
  }

initState :: CompilerState -> IState
initState = IState

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl ()
hoistErr (Right val) = return ()
hoistErr (Left err) =
  liftIO $ print err

updateCompilerState :: CompilerState -> Repl ()
updateCompilerState cs = modify (\st -> st { _compilerState = cs })

-- Execution

exec :: Bool -> CompilerM () -> Repl ()
exec update compM = do
  cs <- gets _compilerState
  (cm, cs'') <- liftIO $ runCompilerM compM cs
  hoistErr cm

  when update (updateCompilerState cs'')
  return ()

execLine :: Bool -> L.Text -> Repl ()
execLine update source = do
  cs <- gets _compilerState
  let cs' = cs { _src = Just source }
  updateCompilerState cs'
  exec update compileLine

execFile :: FilePath -> Repl ()
execFile fname = do
  mtext <- liftIO $ getFileContents fname
  cs <- gets _compilerState
  let cs' = cs { _fname = Just fname, _src = mtext }
  updateCompilerState cs'
  exec True compileFile

cmd :: String -> Repl ()
cmd source = execLine True (L.pack source)

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

load :: [String] -> Repl ()
load [] = showError $ "load requires a filename"
load (fname:[]) = execFile fname
load _ = showError $ "load requires a single filename"

flags :: a -> Repl ()
flags _ = do
  cs <- gets _compilerState
  showMsg $ show $ _flags cs

help :: a -> Repl ()
help _ = showMsg "Commands available \n\
\  :set FLAG\tsets a compiler flag \n\
\  :unset FLAG\tunsets a compiler flag \n\
\  :flags\tprint all set compiler flags \n\
\  :quit\tquit the repl"

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

-- Interactive Shell

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":load", fileCompleter)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds =
        [ ":load"
        , ":set"
        , ":unset"
        , ":flags"
        , ":quit"
        , ":help"
        ]
  return $ filter (isPrefixOf n) cmds

options :: [(String, [String] -> Repl ())]
options =
  [ ("set", set)
  , ("unset", unset)
  , ("flags", flags)
  , ("load", load)
  , ("help", help)
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
  L.putStrLn $ L.pack banner
  shell cs (return ())

