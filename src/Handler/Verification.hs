
module Handler.Verification where

import Import
import Handler.Database
import qualified Language.PureScript.Docs as D

getVerifyR :: VerificationKey -> Handler Html
getVerifyR key = do
  exists <- verificationKeyExists key
  if exists
    then defaultLayout
      [whamlet|
        <form method="post">
          <input type="submit" value="Verify">|]
    else notFound

postVerifyR :: VerificationKey -> Handler Html
postVerifyR key = do
  -- TODO: Github OAuth
  result <- verifyPackage key (D.GithubUser "hdgarrood")
  case result of
    VerifySuccess pkg -> do
      setMessage "Your package was verified successfully."
      redirect (packageRoute pkg)
    VerifyUnknownKey -> do
      -- TODO: This should not be 200 OK.
      defaultLayout
        [whamlet|
          <div .error>
            No pending package was found; check the URL or try uploading again.
          |]
