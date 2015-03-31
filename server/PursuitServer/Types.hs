module PursuitServer.Types where

data ServerOptions = ServerOptions
  { serverPort             :: Int
  , serverLibrariesFile    :: FilePath
  , serverGithubAuthToken  :: Maybe String
  }

