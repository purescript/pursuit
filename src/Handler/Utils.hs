
module Handler.Utils where

import Import
import qualified Data.ByteString.Lazy as BL

badRequest :: Text -> Handler a
badRequest = sendResponseStatus badRequest400

internalServerError :: Handler a
internalServerError = sendResponseStatus internalServerError500 ("" :: Text)

getDataDir :: Handler String
getDataDir = appDataDir . appSettings <$> getYesod

-- | Read the file at the given path as a lazy ByteString, or return Nothing
-- if no such file exists.
readFileMay :: (IOData a) => String -> IO (Maybe a)
readFileMay file =
  catchJust selectDoesNotExist
            (Just <$> readFile (fpFromString file))
            (const (return Nothing))
  where
  selectDoesNotExist e
    | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
    | otherwise = Nothing
