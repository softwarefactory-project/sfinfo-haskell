-- | A clone function
module Sfinfo.Cloner
  ( clone,
    commit,
    gitReview,
  )
where

import Crypto.Hash (Digest, SHA1 (..), hash)
import Data.Maybe
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.URI
import Turtle hiding (stderr, stdout)

-- | Strip scheme, port, auth and query fragments
urlToDir :: Text -> Maybe Text
urlToDir url = do
  uri <- parseURI (unpack url)
  uriAuth <- uriAuthority uri
  return $ replace ".io/r/" ".io/" (pack (uriRegName uriAuth <> uriPath uri))

commit :: MonadIO io => Text -> Text -> io Text
commit gitDir commitMessage =
  do
    void $ Turtle.proc "/bin/git" gitargs mempty
    pure changeIdStr
  where
    gitargs = ["-C", gitDir, "commit", "-a", "-m", commitMessage <> "\n\nChange-Id: " <> changeIdStr]
    changeIdStr :: Text
    changeIdStr = "I" <> pack (show changeId)
    changeId :: Digest SHA1
    changeId = hash (encodeUtf8 (gitDir <> commitMessage))

clone :: MonadIO io => Text -> Text -> io Text
clone base url =
  case urlToDir url of
    Nothing -> error "Invalid url"
    Just urlDir -> do
      gitDirExist <- testdir (fromText (dest <> "/.git"))
      unless gitDirExist (procs "/bin/git" ["clone", url, dest] mempty)
      pure dest
      where
        dest = base <> fromMaybe urlDir (stripSuffix ".git" urlDir)

gitReview :: MonadIO io => Text -> Text -> Text -> io ()
gitReview user projectName gitDir = procs "/bin/git" gitargs mempty
  where
    gitargs = ["-C", gitDir, "push", giturl, gitref]
    giturl = "ssh://" <> user <> "@softwarefactory-project.io:29418/" <> projectName
    gitref = "HEAD:refs/for/master"
