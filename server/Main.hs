{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Version (showVersion)

import Options.Applicative

import PursuitServer.Server
import PursuitServer.Types

import qualified Paths_pursuit as Paths

port :: Parser Int
port = option auto
     ( value 8080
    <> short 'p'
    <> long "port"
    <> help "The port to listen on"
    <> metavar "PORT"
     )

librariesFile :: Parser FilePath
librariesFile = strOption
         ( value "libraries.json"
        <> short 'l'
        <> long "libraries"
        <> help "The libraries file"
        <> metavar "PATH"
         )

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions <$> port
                              <*> librariesFile

main :: IO ()
main = execParser opts >>= runServer
  where
  opts = info (version <*> helper <*> serverOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "pursuit-server - web-based search engine for PureScript code"
  footerInfo  = footer $ "pursuit-server " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
