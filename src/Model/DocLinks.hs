
module Model.DocLinks where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.DeepSeq (NFData)
import Data.List (find)
import Data.Char (isUpper)
import Data.Version
import Data.Text (Text)
import qualified Data.Text as T
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
  , ctxVersionTag           :: Text
  }
  deriving (Show, Eq, Ord)

-- | The NS suffix helps avoid clashes with constructors from the purescript
-- compiler library.
data Namespace
  = TypeNS
  | ValueNS
  | KindNS
  deriving (Show, Eq, Enum, Ord, Generic)

instance NFData Namespace

data DocLink = DocLink
  { linkLocation  :: LinkLocation
  , linkTitle     :: Text
  , linkNamespace :: Namespace
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

  -- | A link to a declaration that is built in to the compiler, e.g. the Prim
  -- module. In this case we only need to store the module that the builtin
  -- comes from (at the time of writing, this will only ever be "Prim").
  | BuiltinModule P.ModuleName
  deriving (Show, Eq, Ord)

-- | Given a links context, a thing to link to (either a value or a type), and
-- its containing module, attempt to create a DocLink.
getLink :: LinksContext -> P.ModuleName -> Text -> ContainingModule -> Maybe DocLink
getLink LinksContext{..} curMn target containingMod = do
  location <- getLinkLocation
  return DocLink
    { linkLocation = location
    , linkTitle = target
    , linkNamespace = namespace
    }

  where
  getLinkLocation = normalLinkLocation <|> builtinLinkLocation

  normalLinkLocation = do
    let bookmark' = (fromContainingModule curMn containingMod, target)
    bookmark <- find ((bookmark' ==) . ignorePackage) ctxBookmarks
    getLocation containingMod bookmark

  getLocation containingModule bookmark =
    case containingModule of
      ThisModule ->
        return SameModule
      OtherModule destMn ->
        case bookmark of
          Local _ ->
            return $ LocalModule curMn destMn
          FromDep pkgName _ -> do
            pkgVersion <- lookup pkgName ctxResolvedDependencies
            return $ DepsModule curMn pkgName pkgVersion destMn

  builtinLinkLocation = do
    let primMn = P.moduleNameFromString "Prim"
    guard $ containingMod == OtherModule primMn
    -- TODO: ensure the declaration exists in the builtin module too
    return $ BuiltinModule primMn

  -- TODO: fix this, it's not correct.
  namespace = case T.unpack target of
    [] ->
      TypeNS -- should never happen, but this will do
    (t:_) ->
      if isUpper t
        then TypeNS
        else ValueNS

getLinksContext :: Package a -> LinksContext
getLinksContext Package{..} =
  LinksContext
    { ctxGithub               = pkgGithub
    , ctxBookmarks            = pkgBookmarks
    , ctxResolvedDependencies = pkgResolvedDependencies
    , ctxPackageName          = bowerName pkgMeta
    , ctxVersion              = pkgVersion
    , ctxVersionTag           = pkgVersionTag
    }
