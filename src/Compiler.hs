{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import           Flags
import           Monad

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Text            as T
import           Data.Text.IO         as TIO
import           System.Directory

compileFile :: CompilerM ()
compileFile = do
  Just fname <- gets _fname
  msrc <- gets _src
  case msrc of
    Nothing -> throwError $ FileNotFound fname
    Just src -> do
      inIO $ TIO.putStrLn $ T.pack $ "Compiling " ++ fname
      inIO $ TIO.putStrLn src


compileLine :: T.Text -> CompilerM ()
compileLine text = do
  inIO $ TIO.putStrLn "Compiling a line"
  inIO $ TIO.putStrLn text
