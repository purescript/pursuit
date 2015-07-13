
module Handler.Help where

import Import
import Handler.Caching

getHelpR :: Handler Html
getHelpR =
  cacheHtml $ do
    suggestedBadgeMarkup <- getSuggestedBadgeMarkup
    defaultLayout $(widgetFile "help")

getSuggestedBadgeMarkup :: Handler Text
getSuggestedBadgeMarkup = do
  root <- appRoot . appSettings <$> getYesod
  let url = (root <>)
  return $ unlines
    [ "<a href=\"" <> url "/packages/$PACKAGE_NAME" <> "\">"
    , "  <img src=\"" <> url "/packages/$PACKAGE_NAME/badge" <> "\""
    , "       alt=\"$PACKAGE_NAME on Pursuit\">"
    , "  </img>"
    , "</a>"
    ]
