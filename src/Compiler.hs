{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import           Control.Monad.Except
import           Control.Monad.State

import           Data.Text.Lazy       as L
import           Data.Text.Lazy.IO    as L
import           System.Directory

import           CompilerError
import           Flags
import qualified Frontend             as Syn
import           Monad
import           Parser
import           Types.Infer

useFile :: FilePath -> CompilerM ()
useFile fname = do
  mtext <- liftIO $ getFileContents fname
  modify (\cs ->  cs { _fname = Just fname,
                    _src = mtext })
    
readAndCompileFile :: FilePath -> IO ClassEnv
readAndCompileFile fname = do
  (Right a, cs) <- runCompilerM cm emptyCS
  return a
  where
    cm = do
      useFile fname
      mod <- compileFile
      case runInfer emptyEnv $ addModuleClasses mod of
        Right (ce) -> return ce
  
compileFile :: CompilerM Syn.Module
compileFile = do
  Just fname <- gets _fname
  Just src <- gets _src
  case parseModule fname src of
    Right mod -> do
      let mod' = Syn.groupTopLevel mod
      ifSet dumpFrontend (dumpValues "Frontend" mod')
      return mod'
    Left s -> throwError $ ParseError s

  -- Just fname <- gets _fname
  -- msrc <- gets _src
  -- case msrc of
  --   Nothing -> throwError $ FileNotFound fname
  --   Just src ->
  --     case parseModule fname src of
  --       Right mod -> do
  --         let mod' = Syn.groupTopLevel mod
  --         ifSet dumpFrontend (dumpValues "Frontend" mod')
  --         return mod'
  --       Left s -> throwError $ ParseError s

compileLine :: CompilerM ()
compileLine = do
  Just text <- gets _src
  ast <- parseText text
  return ()

parseText :: L.Text -> CompilerM Syn.Expr
parseText input = do
  let ast = parseExpr input
  ifSet dumpFrontend (dumpValues "Frontend" ast)
  case ast of
    Right ast' -> return ast'
    Left s     -> throwError $ ParseError s

dumpValues :: Show a => String -> a -> CompilerM ()
dumpValues header v = do
  inIO $ L.putStrLn $ L.pack $ "--- " ++ header
  inIO $ L.putStrLn $ L.pack $ show v
  inIO $ L.putStrLn ""

getFileContents :: FilePath -> IO (Maybe L.Text)
getFileContents fname = do
  exists <- doesFileExist fname
  if exists
    then do
      text <- L.readFile fname
      return $ Just text
    else return Nothing
