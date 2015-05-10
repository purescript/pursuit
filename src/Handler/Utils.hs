
module Handler.Utils where

import Import

badRequest :: Text -> Handler a
badRequest = sendResponseStatus badRequest400

internalServerError :: Handler a
internalServerError = sendResponseStatus internalServerError500 ("" :: Text)
