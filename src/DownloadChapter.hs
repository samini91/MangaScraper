{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module DownloadChapter
  (
    download
  )
where

import Path
import ScraperData
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Control.Concurrent.Async
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.FilePath (dropTrailingPathSeparator)
import Network.Wreq
import Control.Lens
import Scraper
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans.Except (ExceptT)
import Network.HTTP.Client (HttpException (HttpExceptionRequest))
import Control.Exception.Lifted
import System.Log.FastLogger
import Infra (Env(..))
import qualified Codec.Archive.Zip as Zip
import Control.Monad (foldM, when)
import qualified GoogleDrive as GD

-- need to add download path here too for upload?
download :: Env -> DownloadChapterRequest -> IO DownloadInfo
download env u = do
  m <- grabPageRemoveRedundancy env (getMangaWebSiteUrl (link u))
  let log = getMangaWebSiteUrl (link u)
  _ <- env.logFunc $ toLogStr (show log)
  let z = snd m
  let strHtml = T.unpack z
  let x = parseImages (fst m) (parseTags strHtml)
  res <- saveFiles (downloadInfoUrl x)
  let allFiles = sequenceA res
  let downloadInfo = case allFiles of
        Right filePaths -> do
          let zipPath = maybe "manga/unknown.cbz" (\fp -> dropTrailingPathSeparator (fromRelDir fp) ++ ".cbz") filePath
          _ <- createZipFile zipPath filePaths
          _ <- env.logFunc $ toLogStr ("Created zip file: " ++ zipPath)
          case filePath of
            Just fp -> do
              let folderPath = fromRelDir fp
              removeDirectoryRecursive folderPath
              _ <- env.logFunc $ toLogStr ("Deleted folder: " ++ folderPath)
              return ()
            Nothing -> return ()

          -- Attempt upload to Google Drive
          uploadSuccess <- attemptUpload env zipPath zipPath

          -- Delete local CBZ if upload succeeded
          when uploadSuccess $ do
            removeFile zipPath
            _ <- env.logFunc $ toLogStr ("Deleted local file after successful upload: " ++ zipPath)
            return ()

          return (DownloadInfo { downloadInfoUrl = x.downloadInfoUrl })
        Left err -> do
          _ <- env.logFunc $ toLogStr ("Error downloading files: " ++ show err)
          return (DownloadInfo { downloadInfoUrl = x.downloadInfoUrl })
  downloadInfo
  where
      filePath = mangaFilePath (downloadChapterRequestMangaName u) chapterNumber
      saveWithPath y = catchHttpException $ saveFile filePath y
      saveFiles y = mapConcurrently saveWithPath (filter (\x -> urlValue (snd x) /= "http://") y)
      chapterNumber = (addPadding 4 (downloadChapterRequestNumber u)) ++ "_" ++ pageLinkChapterName (downloadChapterRequestLink u)
      link r = pageLinkUrl $ downloadChapterRequestLink r

-- | Attempt to upload file to Google Drive
attemptUpload :: Env -> FilePath -> FilePath -> IO Bool
attemptUpload env localPath drivePath = do
  case driveConfig env of
    Nothing -> do
      -- Drive not configured, skip upload
      return False
    Just config -> do
      _ <- env.logFunc $ toLogStr ("Uploading to Google Drive: " ++ drivePath)
      tokenResult <- GD.getValidToken config
      case tokenResult of
        Left (GD.AuthError err) -> do
          _ <- env.logFunc $ toLogStr ("Google Drive authentication failed: " ++ err ++ ". Tokens may be revoked. Keeping local file.")
          return False
        Left (GD.NetworkError err) -> do
          _ <- env.logFunc $ toLogStr ("Network error uploading to Google Drive: " ++ err ++ ". Keeping local file.")
          return False
        Left (GD.FileNotFound fp) -> do
          _ <- env.logFunc $ toLogStr ("File not found: " ++ fp ++ ". Keeping local file.")
          return False
        Left GD.InvalidTokens -> do
          _ <- env.logFunc $ toLogStr ("Google Drive authentication failed. Tokens invalid. Run auth setup again. Keeping local file." :: String)
          return False
        Right accessToken -> do
          uploadResult <- GD.uploadFile config accessToken localPath drivePath
          case uploadResult of
            Left (GD.NetworkError err) -> do
              _ <- env.logFunc $ toLogStr ("Failed to upload to Google Drive: " ++ err ++ ". Keeping local file.")
              return False
            Left err -> do
              _ <- env.logFunc $ toLogStr ("Failed to upload to Google Drive: " ++ show err ++ ". Keeping local file.")
              return False
            Right (GD.DriveFileId fileId) -> do
              _ <- env.logFunc $ toLogStr ("Successfully uploaded to Google Drive: " ++ fileId)
              return True

createZipFromFiles :: [FilePath] -> IO Zip.Archive
createZipFromFiles filepaths = do
  foldM addFileToArchive Zip.emptyArchive filepaths
  where
    addFileToArchive acc filepath = do
      content <- B.readFile filepath
      return $ Zip.addEntryToArchive (Zip.toEntry filepath 0 content) acc

createZipFile :: FilePath -> [FilePath] -> IO FilePath
createZipFile outputZipPath inputFiles = do
  archive <- createZipFromFiles inputFiles
  B.writeFile outputZipPath (Zip.fromArchive archive)
  return outputZipPath

--createZipFile :: Maybe(Path Rel Dir) -> IO FilePath
--createZipFile pathOfManga = do
  --let successfulImages = [img | Right img <- imageData]
--  let archive = foldr (\(filename, content) acc -> Zip.addEntryToArchive (Zip.toEntry filename 0 content) acc) Zip.emptyArchive successfulImages
--  return createDirectoryIfMissing True (takeDirectory zipFilePath)

catchHttpException:: forall a . IO a -> IO (Either HttpException a) -- pass in the path instead
catchHttpException x = Control.Exception.Lifted.try x  :: IO (Either HttpException a)

saveFile :: Maybe(Path Rel Dir)-> (PageNumber, Url) -> IO FilePath -- pass in the path instead 
saveFile p x = do
  m <- get (urlValue $ snd x)
  let z = m ^. responseBody
  createDirectoryIfMissing True filePath -- get string representation here return FilePath of "" if maybe is empty
  B.writeFile fileName z
  return fileName
  where
    filePath = case (fromRelDir <$> p) of
                 Nothing -> ""
                 Just a -> a
    fileName = filePath ++ (fst x) ++ ".jpg" -- should wrap this in a monad too

-- katana
parseImagesKatana :: [Tag String] -> DownloadInfo
parseImagesKatana tags =
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "data-src" "img") tags
    scrapeImagesWithPageNumber = zip ((addPadding 4) <$> [0..]) scrapeImages
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


-- possible padding is less than len I guess we should do nothing in that case
addPadding :: Int -> Int -> String
addPadding paddingAmt i =
  let
    s = show i
    len = length s
   in
    duplicate "0" (paddingAmt - len) ++ s


-- kakalot
parseImagesKakalot :: [Tag String] -> DownloadInfo
parseImagesKakalot tags =
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "src" "img") tags
    scrapeImagesWithPageNumber = zip ((addPadding 4) <$> [0..]) scrapeImages
--    scrapeImagesWithPageNumber = zip [0..] scrapeImages
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []


parseImages :: MangaWebSite -> [Tag String] -> DownloadInfo
parseImages (MangaKatana _) l = parseImagesKatana l
parseImages (MangaKakalot _) l = parseImagesKakalot l

mangaFilePath :: String -> String -> Maybe (Path Rel Dir)
mangaFilePath a b = do
  _0 <- parseRelDir "manga"
  _1 <- parseRelDir a
  _2 <- parseRelDir b
  return (_0 </> _1 </> _2)






--mangaFilePath :: String -> String -> String -> Maybe (Path Rel Dir)
--mangaFilePath a b c = do
--  _0 <- (parseRelDir "manga")
--  _1 <- (parseRelDir a)
--  _2 <- (parseRelDir b)
--  _3 <- (parseRelDir c)
--  return (_0 </> _1 </> _2 </> _3)
