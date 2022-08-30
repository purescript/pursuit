-- To work around a bug in GHC 8.0.1:
-- See https://groups.google.com/forum/#!topic/yesodweb/DlyXqFM7ZnY
{-# LANGUAGE NoDisambiguateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation
import Language.PureScript.CoreFn.FromJSON (parseVersion')
import Text.Read (readsPrec)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Text.Julius                 (rawJS)
import Yesod.Core.Types
    ( Logger, HandlerData, rheSite, handlerEnv )
import Yesod.EmbeddedStatic        (EmbeddedStatic, embedStaticContent)
import qualified Yesod.Core.Unsafe as Unsafe

import Web.Bower.PackageMeta (PackageName, parsePackageName, runPackageName)
import Data.Version
import qualified Language.PureScript.Docs as D
import qualified Paths_pursuit as Paths

import SearchIndex (SearchIndex)

newtype PathPackageName =
  PathPackageName { runPathPackageName :: PackageName }
  deriving (Show, Eq, Ord)

instance Read PathPackageName where
  readsPrec _ str =
    case parsePackageName (pack str) of
      Right n -> [(PathPackageName n, "")]
      Left _ -> []

instance PathPiece PathPackageName where
  toPathPiece = runPackageName . runPathPackageName
  fromPathPiece = fmap PathPackageName . hush . parsePackageName

newtype PathVersion =
  PathVersion { runPathVersion :: Version }
  deriving (Show, Eq, Ord, Read)

instance PathPiece PathVersion where
  toPathPiece = toPathPiece . showVersion . runPathVersion
  fromPathPiece = fmap PathVersion . parseVersion' . T.unpack

-- | A base64 encoded string.
newtype VerificationKey =
  VerificationKey { runVerificationKey :: ByteString }
  deriving (Show, Eq, Ord, Read)

instance PathPiece VerificationKey where
  toPathPiece = decodeUtf8 . runVerificationKey
  fromPathPiece = Just . VerificationKey . encodeUtf8


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings       :: AppSettings
    , appStatic         :: EmbeddedStatic
    -- ^ Settings for static file serving.
    , appHttpManager    :: Manager
    , appLogger         :: Logger
    , appSearchIndex    :: TVar SearchIndex
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is an orphan instance; previously it seems to have been provided by
-- Yesod, but it appears to no longer exist as of Yesod 1.6. Therefore, we
-- provide it here instead.
instance HasHttpManager site => HasHttpManager (HandlerData child site) where
    getHttpManager = getHttpManager . rheSite . handlerEnv

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> envClientSessionBackend
        120    -- timeout in minutes
        "PURSUIT_CLIENT_SESSION_KEY"

    defaultLayout widget = do
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        manalytics <- appAnalytics . appSettings <$> getYesod
        isSearch <- testCurrentRoute (== SearchR)
        searchText <- map (fromMaybe "") (lookupGetParam "q")
        let pursuitVersion = showVersion Paths.version
        pc <- widgetToPageContent $ do
          $(widgetFile "default-layout")
          case manalytics of
            Just analytics -> $(widgetFile "analytics")
            _ -> return ()

        let pageTitle' =
              let renderedTitle = Blaze.renderHtml (pageTitle pc)
              in toHtml (if LT.null renderedTitle
                           then "Pursuit"
                           else renderedTitle <> " - Pursuit")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Routes not requiring authentication.
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    addStaticContent = embedStaticContent appStatic StaticR minifym

    makeLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

packageNameRoute :: PackageName -> Route App
packageNameRoute pkgName =
  PackageR (PathPackageName pkgName)

packageRoute :: D.VerifiedPackage -> Route App
packageRoute pkg =
  PackageVersionR (PathPackageName (D.packageName pkg))
                  (PathVersion (D.pkgVersion pkg))

packageDocsRoute :: D.VerifiedPackage -> Route App
packageDocsRoute pkg =
  PackageVersionDocsR (PathPackageName (D.packageName pkg))
                      (PathVersion (D.pkgVersion pkg))

moduleDocsRoute :: D.VerifiedPackage -> Text -> Route App
moduleDocsRoute pkg moduleName =
  PackageVersionModuleDocsR (PathPackageName (D.packageName pkg))
                            (PathVersion (D.pkgVersion pkg))
                            moduleName

substituteVersion :: Route App -> Version -> Route App
substituteVersion route version' =
  let version = PathVersion version'
  in case route of
    PackageVersionR pkgName _ ->
      PackageVersionR pkgName version
    PackageVersionDocsR pkgName _ ->
      PackageVersionDocsR pkgName version
    PackageVersionModuleDocsR pkgName _ modName ->
      PackageVersionModuleDocsR pkgName version modName
    other ->
      other

-- | Check whether the current route satisfies a predicate
testCurrentRoute :: (Route App -> Bool) -> Handler Bool
testCurrentRoute p = map (maybe False p) getCurrentRoute
