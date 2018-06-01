{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cli where

import           Compiler
import           Flags
import           Monad

import           Control.Monad.State

import           Data.Semigroup      ((<>))
import           Data.Text           as T
import           Data.Text.IO        as TIO
import           Options.Applicative
import           System.Directory

data LineOpts
  = UseReplLineOpts
  | RunFileLineOpts String
  deriving (Eq, Show)

data Options = Options
  { lineOpt :: LineOpts
  , flags   :: Flags.Flags
  } deriving (Eq, Show)

parseLineOpts :: Parser LineOpts
parseLineOpts = runFileOpt <|> runReplOpt
  where
    runFileOpt =
      RunFileLineOpts <$> strOption (  long "file"
                                    <> short 'f'
                                    <> metavar "FILE"
                                    <> help "File containing a script to run")
    runReplOpt =
      UseReplLineOpts <$ flag' () (  long "repl"
                                  <> short 'r'
                                  <> help  "Run the interactive REPL")


parseOptions :: Parser Options
parseOptions = Options <$> parseLineOpts <*> parseFlags

runFile :: CompilerState -> FilePath -> IO ()
runFile compilerState fname = do
  mtext <- getFileContents fname
  let updatedState = compilerState { _fname = Just fname, _src = mtext }

  (res, _) <- runCompilerM compileFile updatedState
  case res of
    Left err -> print err
    Right _  -> TIO.putStrLn "fuck yeah"

getFileContents :: FilePath -> IO (Maybe T.Text)
getFileContents fname = do
  exists <- doesFileExist fname
  if exists
    then do
      text <- TIO.readFile fname
      return $ Just text
    else return Nothing

styxEntry :: Options -> IO ()
styxEntry opts =
  let
    compilerState = emptyCS { _flags = flags opts }
  in
    case lineOpt opts of
        UseReplLineOpts       -> TIO.putStrLn "repl"
        RunFileLineOpts fname ->
          runFile compilerState fname

cliIFace :: IO ()
cliIFace = execParser opts >>= styxEntry
  where
    opts = info (helper <*> parseOptions)
      (  fullDesc
      <> header "The Styx Language"
      <> progDesc "Repl and Interpreter for the Styx programming language.")
