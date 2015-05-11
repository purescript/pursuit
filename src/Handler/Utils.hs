
module Handler.Utils where

import Import
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

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
  catchDoesNotExist (readFile (fpFromString file))

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist act =
  catchJust selectDoesNotExist
            (Just <$> act)
            (const (return Nothing))
  where
  selectDoesNotExist e
    | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
    | otherwise = Nothing

writeFileWithParents :: (IOData a) => String -> a -> IO ()
writeFileWithParents file contents = do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile (fpFromString file) contents
