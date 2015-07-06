
module Handler.Utils where

import Import
import TimeUtils
import Web.Cookie (setCookieName, setCookieValue, setCookieMaxAge)
import System.Directory (createDirectoryIfMissing, removeFile,
                        getDirectoryContents, getModificationTime)
import System.FilePath (takeDirectory)

badRequest :: Text -> Handler a
badRequest = sendResponseStatus badRequest400

internalServerError :: Handler a
internalServerError = sendResponseStatus internalServerError500 ("" :: Text)

getDataDir :: Handler String
getDataDir = appDataDir . appSettings <$> getYesod

-- | Read the file at the given path as a lazy ByteString, or return Nothing
-- if no such file exists.
readFileMay :: (IOData a) => FilePath -> IO (Maybe a)
readFileMay file =
  catchDoesNotExist (readFile file)

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist act =
  catchJust selectDoesNotExist
            (Just <$> act)
            (const (return Nothing))
  where
  selectDoesNotExist e
    | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
    | otherwise = Nothing

writeFileWithParents :: (IOData a, MonadIO m) => FilePath -> a -> m ()
writeFileWithParents file contents = liftIO $ do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file contents

deleteFilesOlderThan :: forall m.
  (MonadIO m, MonadLogger m, MonadBaseControl IO m) =>
  NominalDiffTime -> FilePath -> m ()
deleteFilesOlderThan maxAge dir = do
  files <- getDirectoryContents' dir
  filesWithTimes <- liftIO $ traverse (\f -> (f,) <$> getAge f) files
  otraverse_ (\(f, age) -> when (age > maxAge) (tryRemoveFile f)) filesWithTimes
  where
  getAge f = getModificationTime f >>= getElapsedTimeSince

  tryRemoveFile = flip catch logIOException . liftIO . removeFile

  logIOException :: IOException -> m ()
  logIOException e = $logError (tshow e)

-- | Like getDirectoryContents, except that:
--  * It includes the directory that you supplied, so that you can safely pass
--    the results to readFile etc
--  * It removes "." and ".." from the results
--  * It works for any MonadIO
getDirectoryContents' :: forall m. MonadIO m => FilePath -> m [FilePath]
getDirectoryContents' dir =
  liftIO $
    map (dir ++) . filter (`onotElem` [".", ".."]) <$> getDirectoryContents dir

-- | Sets a message which is displayed just once, at the next time the user's
-- browser renders a page.
setCookieMessage :: ByteString -> Handler ()
setCookieMessage msg =
  setCookie def { setCookieName = "message"
                , setCookieValue = msg
                , setCookieMaxAge = Just $ secondsToDiffTime 3600
                }
