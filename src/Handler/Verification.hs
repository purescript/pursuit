
module Handler.Verification where

import Import
import Data.Version
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (runPackageName)

import Handler.Database
import Handler.GithubOAuth

-- This is just here so that we get a CSRF token
confirmVerifyForm :: Form ()
confirmVerifyForm = renderDivs $ pure ()

getVerifyR :: VerificationKey -> Handler Html
getVerifyR = renderVerifyFormWithMessage Nothing

renderVerifyFormWithMessage :: Maybe Text -> VerificationKey -> Handler Html
renderVerifyFormWithMessage errorMessage key = do
  requireAuthentication $ \user -> do
    mpkg <- lookupPendingPackage key
    case mpkg of
      Just pkg@D.Package{..} ->
        let pkgName = runPackageName (D.packageName pkg)
        in do
          (formWidget, enctype) <- generateFormPost confirmVerifyForm
          defaultLayout $(widgetFile "verify")
      Nothing -> notFound

postVerifyR :: VerificationKey -> Handler Html
postVerifyR key = do
  requireAuthentication $ \user -> do
    ((formResult, _), _) <- runFormPost confirmVerifyForm
    case formResult of
      FormSuccess _ -> do
        result <- verifyPackage key user
        case result of
          VerifySuccess pkg -> do
            setMessage "Your package was verified successfully."
            redirect (packageRoute pkg)
          VerifyUnknownKey -> do
            html <- defaultLayout [whamlet|
                      <div .message .error>
                        No pending package was found; check the URL or try uploading again.
                      |]
            sendResponseStatus notFound404 html
      _ ->
        -- note: if we get here, it probably means the CSRF token was invalid.
        renderVerifyFormWithMessage
          (Just "There was a problem with the form. Please try again.")
          key
