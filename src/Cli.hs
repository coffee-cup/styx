{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cli where

import Flags

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

runScript :: FilePath -> IO ()
runScript fname = do
  exists <- doesFileExist fname
  if exists
    then TIO.readFile fname >>= runFile fname
    else TIO.putStrLn "File does not exist."

runFile :: FilePath -> T.Text -> IO ()
runFile filePath fileExpr =
  TIO.putStrLn $ T.pack $ "Running file " ++ filePath

styxEntry :: Options -> IO ()
styxEntry opts =
  case lineOpt opts of
    UseReplLineOpts       -> TIO.putStrLn "repl"
    RunFileLineOpts fname -> do
      TIO.putStrLn $ T.pack $ "file " ++ fname

cliIFace :: IO ()
cliIFace = execParser opts >>= styxEntry
  where
    opts = info (helper <*> parseOptions)
      (  fullDesc
      <> header "The Styx Language"
      <> progDesc "Repl and Interpreter for the Styx programming language.")
