module Foundation where

import Import.NoFoundation
import Text.Read (readsPrec)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import Crypto.Random

import Web.Bower.PackageMeta (PackageName, parsePackageName, runPackageName)
import Data.Version
import qualified Css
import qualified Language.PureScript.Docs as D

newtype PathPackageName =
  PathPackageName { runPathPackageName :: PackageName }
  deriving (Show, Eq, Ord)

instance Read PathPackageName where
  readsPrec _ str =
    case parsePackageName str of
      Right n -> [(PathPackageName n, "")]
      Left _ -> []

instance PathPiece PathPackageName where
  toPathPiece =
    T.pack . runPackageName . runPathPackageName
  fromPathPiece =
    fmap PathPackageName . either (const Nothing) Just . parsePackageName . T.unpack

newtype PathVersion =
  PathVersion { runPathVersion :: Version }
  deriving (Show, Eq, Ord, Read)

instance PathPiece PathVersion where
  toPathPiece = toPathPiece . showVersion . runPathVersion
  fromPathPiece = fmap PathVersion . D.parseVersion' . T.unpack

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
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appCPRNG       :: TVar SystemRNG
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_normalize_css
            $(widgetFile "default-layout")

        let pageTitle' =
              let renderedTitle = Blaze.renderHtml (pageTitle pc)
              in toHtml (if LT.null renderedTitle
                           then "Pursuit"
                           else renderedTitle <> " - Pursuit")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Routes not requiring authenitcation.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- queryDb :: (PursuitDatabase -> a) -> HandlerT App IO a
-- queryDb f = do
--   tvar <- appDatabase <$> getYesod
--   db <- liftIO (readTVarIO tvar)
--   return (f db)

-- updateDb :: (PursuitDatabase -> PursuitDatabase) -> HandlerT App IO ()
-- updateDb f = do
--   tvar <- appDatabase <$> getYesod
--   liftIO (atomically (modifyTVar tvar f))

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

moduleDocsRoute :: D.VerifiedPackage -> String -> Route App
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
