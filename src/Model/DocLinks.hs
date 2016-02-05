
module Model.DocLinks where

import Prelude
import Data.List (find)
import Data.Version
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Arrow (second)

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
  , ctxVersionTag           :: String
  }
  deriving (Show, Eq, Ord)

-- | A LinksContext with the current module name.
type LinksContext' = (LinksContext, P.ModuleName)

data TypeOrValue
  = Type
  | Value
  deriving (Show, Eq, Enum, Ord)

data DocLink = DocLink
  { linkLocation    :: LinkLocation
  , linkTitle       :: String
  , linkTypeOrValue :: TypeOrValue
  }
  deriving (Show, Eq, Ord)

data LinkLocation
  -- | A link to a declaration in the same module.
  = SameModule

  -- | A link to a declaration in a different module, but still in the current
  -- package; we need to store the current module and the other declaration's
  -- module.
  | LocalModule P.ModuleName P.ModuleName

  -- | A link to a declaration in a different package. We store: current module
  -- name, name of the other package, version of the other package, and name of
  -- the module in the other package that the declaration is in.
  | DepsModule P.ModuleName PackageName Version P.ModuleName
  deriving (Show, Eq, Ord)

-- | Given a links context, a type constructor, and its containing module,
-- attempt to create a DocLink for that type constructor.
getLink :: LinksContext' -> String -> ContainingModule -> Maybe DocLink
getLink (LinksContext{..}, curMn) ctor' containingMod = do
  let bookmark' = (fromContainingModule curMn containingMod, ctor')
  bookmark <- find ((bookmark' ==) . ignorePackage) ctxBookmarks
  loc <- getLocation containingMod bookmark
  return $ DocLink
    { linkLocation    = loc
    , linkTitle       = ctor'
    , linkTypeOrValue = Type
    }

  where
  getLocation containingModule bookmark =
    case containingModule of
      ThisModule -> return SameModule
      OtherModule destMn ->
        case bookmark of
          Local _ -> return $ LocalModule curMn destMn
          FromDep pkgName _ -> do
          pkgVersion <- lookup pkgName ctxResolvedDependencies
          return $ DepsModule curMn pkgName pkgVersion destMn

getLinksContext :: Package a -> LinksContext
getLinksContext Package{..} =
  LinksContext
    { ctxGithub               = pkgGithub
    , ctxBookmarks            = primBookmarks ++ pkgBookmarks
    , ctxResolvedDependencies = primDependency : pkgResolvedDependencies
    , ctxPackageName          = bowerName pkgMeta
    , ctxVersion              = pkgVersion
    , ctxVersionTag           = pkgVersionTag
    }

primPackageName :: PackageName
primPackageName = x
  where
  Right x = parsePackageName "purescript-prim"

primBookmarks :: [Bookmark]
primBookmarks =
  map (FromDep primPackageName . second show . toPair . fst) $ M.toList P.primTypes
  where
  toPair (P.Qualified mn x) = (fromJust mn, x)

primDependency :: (PackageName, Version)
primDependency = (primPackageName, Version [0,8,0] []) -- hardcoded for now
