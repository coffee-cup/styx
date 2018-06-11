{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import           Flags
import           Monad
import           Parser

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Text.Lazy       as L
import           Data.Text.Lazy.IO    as L
import           System.Directory

compileFile :: CompilerM ()
compileFile = do
  Just fname <- gets _fname
  msrc <- gets _src
  case msrc of
    Nothing -> throwError $ FileNotFound fname
    Just src -> do
      inIO $ L.putStrLn $ L.pack $ "Compiling " ++ fname
      inIO $ L.putStrLn src

compileLine :: CompilerM ()
compileLine = do
  Just text <- gets _src
  parseText text
  -- inIO $ L.putStrLn "Compiling a line"
  -- inIO $ L.putStrLn text

parseText :: L.Text -> CompilerM ()
parseText input = do
  let tokens = parseTokens $ L.unpack input
  inIO $ L.putStrLn $ L.pack $ show tokens
