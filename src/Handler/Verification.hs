
module Handler.Verification where

import Import
import Data.Version
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (runPackageName)

import Handler.Database
import Handler.GithubOAuth

getVerifyR :: VerificationKey -> Handler Html
getVerifyR key = do
  requireAuthentication $ \user -> do
    mpkg <- lookupPendingPackage key
    case mpkg of
      Just pkg@D.Package{..} ->
        let pkgName = runPackageName (D.packageName pkg)
        in  defaultLayout $(widgetFile "verify")
      Nothing -> notFound

postVerifyR :: VerificationKey -> Handler Html
postVerifyR key = do
  requireAuthentication $ \user -> do
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
