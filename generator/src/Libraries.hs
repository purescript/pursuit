module Libraries where

type GitUrl = String
data Library = Library { libraryGitUrl :: GitUrl
                       , libraryBowerName :: Maybe String }

libraries :: [Library]
libraries =
  [ purescript "identity"
  , purescript "foldable-traversable"
  , purescript "transformers"
  , purescript "monoid"

  , purescriptContrib "canvas"
  , purescriptContrib "virtual-dom"
  ]

  where
  l bower git = Library git (Just bower)
  prefix str = "purescript-" ++ str
  github user repo = "https://github.com/" ++ user ++ "/" ++ repo

  purescript name =
    l (prefix name) (github "purescript" (prefix name))
  purescriptContrib name =
    l (prefix name) (github "purescript-contrib" (prefix name))
