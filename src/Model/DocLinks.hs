
module Model.DocLinks where

import Prelude
import Control.Arrow (second)
import Control.DeepSeq (NFData)
import Data.List (find)
import Data.Char (isUpper)
import Data.Version
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Generics (Generic)

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

data TypeOrValue
  = Type
  | Value
  deriving (Show, Eq, Enum, Ord, Generic)

instance NFData TypeOrValue

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

-- | Given a links context, a thing to link to (either a value or a type), and
-- its containing module, attempt to create a DocLink.
getLink :: LinksContext -> P.ModuleName -> String -> ContainingModule -> Maybe DocLink
getLink LinksContext{..} curMn target containingMod = do
  let bookmark' = (fromContainingModule curMn containingMod, target)
  bookmark <- find ((bookmark' ==) . ignorePackage) ctxBookmarks
  loc <- getLocation containingMod bookmark
  return DocLink
    { linkLocation    = loc
    , linkTitle       = target
    , linkTypeOrValue = typeOrValue
    }

  where
  getLocation containingModule bookmark =
    case containingModule of
      ThisModule -> return SameModule
      OtherModule destMn ->
        case bookmark of
          Local _ ->
            return $ LocalModule curMn destMn
          FromDep pkgName _ -> do
            pkgVersion <- lookup pkgName ctxResolvedDependencies
            return $ DepsModule curMn pkgName pkgVersion destMn

  typeOrValue = case target of
    [] ->
      Type -- should never happen, but this will do
    (t:_) ->
      if isUpper t
        then Type
        else Value

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
  map (FromDep primPackageName . second (T.unpack . P.runProperName) . toPair . fst) $ M.toList P.primTypes
  where
  toPair (P.Qualified mn x) = (fromJust mn, x)

primDependency :: (PackageName, Version)
primDependency = (primPackageName, Version [0,8,0] []) -- hardcoded for now
