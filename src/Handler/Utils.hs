
module Handler.Utils where

import Import
import TimeUtils
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
  filesWithTimes <- liftIO $ do
    files <- map (dir ++) <$> getDirectoryContents dir
    traverse (\f -> (f,) <$> getAge f) files

  otraverse_ (\(f, age) -> when (age > maxAge) (tryRemoveFile f)) filesWithTimes
  where
  getAge f = getModificationTime f >>= getElapsedTimeSince

  tryRemoveFile = flip catch logIOException . liftIO . removeFile

  logIOException :: IOException -> m ()
  logIOException e = $logError (tshow e)
