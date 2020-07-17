-- |
module Sfinfo.RpmSpec
  ( bumpVersion,
    getSpec,
    getDate,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (listDirectory)

getSpec :: Text -> IO (FilePath, Text)
getSpec gitDir = do
  files <- listDirectory $ T.unpack gitDir
  case filter (T.isSuffixOf ".spec") $ map T.pack files of
    [file] -> do
      let filePath = T.unpack $ gitDir <> "/" <> file
      fileContent <- T.readFile filePath
      pure (filePath, fileContent)
    _ -> error $ T.unpack gitDir <> ": more (or less) than one spec: " <> show files

getDate :: IO Text
getDate = T.pack <$> (formatTime defaultTimeLocale "%a %b %e %Y" <$> getCurrentTime)

-- | Bump version in spec file
-- >>> bumpVersion "4.2.0" "test" "tata" (T.unlines ["Name: test-package", "Version:     1.3.0"])
-- "Name: test-package\nVersion:     4.2.0\n"
bumpVersion :: Text -> Text -> Text -> Text -> Text
bumpVersion version author date spec = T.unlines $ go False (T.lines spec)
  where
    go :: Bool -> [Text] -> [Text]
    go _ [] = []
    go dirty (x : xs)
      | isVersion x = bumpVersion' x : go (versionChanged x) xs
      | isRelease x = resetRelease dirty x : go dirty xs
      | isChangelog x = addChangelog dirty x : go dirty xs
      | otherwise = x : go dirty xs
    isVersion :: Text -> Bool
    isVersion l = T.isPrefixOf "version: " (T.toLower l)
    isRelease l = T.isPrefixOf "release: " (T.toLower l)
    isChangelog l = T.toLower l == "%changelog"
    getVersion = T.dropWhile (not . isDigit)
    getRelease = T.dropWhile (== ' ') . T.dropWhile (/= ' ')
    bumpVersion' :: Text -> Text
    bumpVersion' l = T.replace (getVersion l) version l
    versionChanged l = getVersion l /= version
    resetRelease :: Bool -> Text -> Text
    resetRelease False l = l
    resetRelease True l = T.replace (getRelease l) "1%{?dist}" l
    addChangelog False x = x
    addChangelog True _ =
      T.unlines
        [ "%changelog",
          "* " <> date <> " " <> author <> " - " <> version <> "-1",
          "- Bump to " <> version
        ]
