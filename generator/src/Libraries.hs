module Libraries where

type GitUrl = String
data Library = Library { libraryGitUrl :: GitUrl
                       , libraryBowerName :: Maybe String }

libraries :: [Library]
libraries =
  [ l "purescript-identity" (githubPs "purescript-identity")
  ]

  where
  l bower git = Library git (Just bower)
  github user repo = "https://github.com/" ++ user ++ "/" ++ repo
  githubPs = github "purescript"
  githubPsContrib = github "purescript-contrib"
