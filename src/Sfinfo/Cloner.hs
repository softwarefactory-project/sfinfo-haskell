-- | A clone function
module Sfinfo.Cloner
  ( clone,
    commit,
    gitReview,
    reset,
    mkChangeId,
    urlToGitDir,
    isClean,
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

mkChangeId :: Text -> Text -> Text
mkChangeId gitDir commitMessage = "I" <> pack (show changeId)
  where
    changeId :: Digest SHA1
    changeId = hash (encodeUtf8 (gitDir <> commitMessage))

git :: MonadIO io => Text -> [Text] -> io ()
git gitDir args = void $ Turtle.proc "/bin/git" (["-C", gitDir] <> args) mempty

reset :: MonadIO io => Text -> Text -> io ()
reset gitDir ref = do
  git gitDir ["clean", "-x", "-f", "-d"]
  git gitDir ["fetch", "origin"]
  git gitDir ["reset", "--hard", ref]

isClean :: MonadIO io => Text -> Text -> io Bool
isClean gitDir branch = do
  exitCode <- Turtle.proc "/bin/git" (["-C", gitDir] <> gitArgs) mempty
  pure $ case exitCode of
    ExitSuccess -> True
    _ -> False
  where
    gitArgs = ["diff", "--exit-code", "HEAD..origin/" <> branch]

commit :: MonadIO io => Text -> Text -> Text -> io ()
commit gitDir commitMessage changeId = git gitDir gitArgs
  where
    gitArgs = ["commit", "-a", "-m", commitMessage <> "\n\nChange-Id: " <> changeId]

urlToGitDir :: Text -> Text -> Text
urlToGitDir base url =
  case urlToDir url of
    Nothing -> error "Invalid url"
    Just urlDir -> base <> fromMaybe urlDir (stripSuffix ".git" urlDir)

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
