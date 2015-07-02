
module Model.DocLinks where

import Prelude
import Data.List (find)
import Data.Version

import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)

data LinksContext = LinksContext
  { ctxGithub               :: (GithubUser, GithubRepo)
  , ctxBookmarks            :: [Bookmark]
  , ctxResolvedDependencies :: [(PackageName, Version)]
  , ctxPackageName          :: PackageName
  , ctxVersion              :: Version
  }
  deriving (Show, Eq, Ord)

-- | A LinksContext with the current module name.
type LinksContext' = (LinksContext, P.ModuleName)

data DocLink
  -- | A link to a declaration in the same module; in this case, only the title
  -- is needed to generate the link.
  = SameModule String

  -- | A link to a declaration in a different module, but still in the current
  -- package; we need to store the current module, the other declaration's
  -- module, and the declaration title.
  | LocalModule P.ModuleName P.ModuleName String

  -- | A link to a declaration in a different package. We store: current module
  -- name, name of the other package, version of the other package, name of
  -- the module in the other package that the declaration is in, and
  -- declaration title.
  | DepsModule P.ModuleName PackageName Version P.ModuleName String
  deriving (Show, Eq, Ord)

-- | Given a links context, a type constructor, and its containing module,
-- attempt to create a DocLink for that type constructor.
getLink :: LinksContext' -> String -> ContainingModule -> Maybe DocLink
getLink (LinksContext{..}, curMn) ctor' containingMod = do
  let bookmark' = (fromContainingModule curMn containingMod, ctor')
  bookmark <- find ((bookmark' ==) . ignorePackage) ctxBookmarks

  case containingMod of
    ThisModule -> return (SameModule ctor')
    OtherModule destMn ->
      case bookmark of
        Local _ -> return (LocalModule curMn destMn ctor')
        FromDep pkgName _ -> do
          pkgVersion <- lookup pkgName ctxResolvedDependencies
          return (DepsModule curMn pkgName pkgVersion destMn ctor')

getLinksContext :: Package a -> LinksContext
getLinksContext Package{..} =
  LinksContext
    { ctxGithub               = pkgGithub
    , ctxBookmarks            = pkgBookmarks
    , ctxResolvedDependencies = pkgResolvedDependencies
    , ctxPackageName          = bowerName pkgMeta
    , ctxVersion              = pkgVersion
    }
