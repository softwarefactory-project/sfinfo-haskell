module Sfinfo
  ( proposeUpdate,
    comparePipAndRpm,
  )
where

import Control.Monad (forM_, unless, void)
import qualified Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Gerrit (GerritClient)
import qualified Gerrit
import Podman (containerRunning, containerState, inspectContainer, isContainer)
import Sfinfo.Cloner (clone, commit, gitReview)
import Sfinfo.PipNames (ignoreList, pipList)
import Sfinfo.RpmSpec (bumpVersion, getDate, getSpec)
import SimpleCmd (cmd, cmdMaybe, cmd_)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.PrettyPrint.ANSI.Leijen (green, putDoc, text)
import Turtle (FilePath, encodeString)
import Prelude hiding (FilePath, id)

-- | A breakOn that drops the seprator
-- >>> breakOn' " -> " "a -> b"
-- ("a","b")
breakOn' :: Text -> Text -> (Text, Text)
breakOn' sep txt = (a, T.drop (T.length sep) b)
  where
    (a, b) = T.breakOn sep txt

-- | Read pkgtree diff output
-- >>> readOutdatedLine "ansible: 2.6.19-2.el7 -> 2.9.10-1.el7"
-- ("ansible","2.9.10")
readOutdatedLine :: Text -> (Text, Text)
readOutdatedLine line = (package, version)
  where
    (package, versions) = breakOn' ": " line
    (_currentVersion, desiredVersion) = breakOn' " -> " versions
    (version, _release) = breakOn' "-" desiredVersion

author :: Text
author = "sfinfo <softwarefactory-dev@redhat.com>"

-- | Creat the bump version commit and return the changeID
commitUpdate :: Text -> Text -> IO Text
commitUpdate gitDir version =
  do
    (specPath, specContent) <- getSpec gitDir
    date <- getDate
    T.writeFile specPath (bumpVersion version author date specContent)
    commit gitDir commitTitle
  where
    commitTitle = "Bump to " <> version

updatePackage :: GerritClient -> Text -> Text -> (Text, Text) -> IO (Text, Maybe Text)
updatePackage client home gerritUser (name, version) =
  do
    gitDir <- clone gitBase projectUrl
    changeId <- commitUpdate gitDir version
    gerritChanges <- queryGerrit changeId
    result <- case gerritChanges of
      [] -> do
        gitReview gerritUser projectName gitDir
        pure $ Just "review submited"
      [change@Gerrit.GerritChange {..}] ->
        if status == Gerrit.MERGED
          then pure Nothing -- Change is merged, nothing to report
          else pure $ Just $ changeUrl change
      -- Is there a +2 ?
      -- Is the zuul gate sf-master queue length < 2 ?
      -- Then perhaps add +2/+A
      _ -> pure $ Just $ T.pack $ "multiple review opened: " <> show (map changeUrl gerritChanges)
    pure (name, result)
  where
    changeUrl = Gerrit.changeUrl client
    queryGerrit changeId = Gerrit.queryChanges [Gerrit.Project projectName, Gerrit.ChangeId changeId] client
    gitBase = home <> "/src/"
    projectName = "rpms/" <> T.replace "python3-" "python-" name
    projectUrl = "https://softwarefactory-project.io/r/" <> projectName

proposeUpdate :: Text -> Text -> FilePath -> IO ()
proposeUpdate home gerritUser fn = Gerrit.withClient "https://softwarefactory-project.io/r" $ \client -> do
  print $ "Proposing update using: " <> fn
  fcontent <- T.readFile (encodeString fn)
  results <- mapM (updatePackage client home gerritUser . readOutdatedLine) $ T.lines fcontent
  putDoc (green (text "Summary:\n"))
  forM_ (filter (isJust . snd) results) $ \(name, message) ->
    putStrLn $ T.unpack $ name <> ": " <> fromMaybe "N/A" message

-- | Convenient bind unless wrapper for the `if "test in IO" then "do this IO" pattern`
--
-- unless        :: Applicative f => Bool -> f () -> f ()
-- flip          ::    (a -> b -> c) -> b -> a    -> c
-- (flip unless) :: Applicative f => f () -> Bool -> f ()
--
-- For example to print something if a file does not exist:
-- (flip unless (print "Nop")) :: Bool     -> IO ()
-- (doesFileExist)             :: FilePath -> IO Bool
-- (doesFileExist "/test" >>=) :: (Bool -> IO b) -> IO b
-- >>> doesFileExist "/test" `bunless` print "Nop!"
-- "Nop!"
bunless :: IO Bool -> IO () -> IO ()
a `bunless` b = a >>= flip unless b

-- fixity value ensure this get evaluated last
infixl 0 `bunless`

-- | Install zuul in a venv using pip
getPipList :: IO [String]
getPipList =
  do
    doesDirectoryExist "venv" `bunless` cmd_ "python3" ["-m", "venv", "venv"]
    doesFileExist "/usr/include/re2/re2.h" `bunless` cmd_ "sudo" ["dnf", "install", "-y", "re2-devel"]
    mapM_ ensure ["zuul", "nodepool", "ansible"]
    lines <$> cmd "./venv/bin/pip3" ["freeze"]
  where
    ensure name = doesFileExist ("venv/bin/" <> name) `bunless` cmd_ "./venv/bin/pip3" ["install", name]

convertPipFreezeToRpmQa :: String -> Maybe String
convertPipFreezeToRpmQa name' =
  case T.splitOn "==" (T.pack name') of
    [name, version] ->
      if name `elem` ignoreList
        then Nothing
        else Just $ T.unpack $ convert name <> "-" <> version <> fakeRelease
    _ -> Nothing
  where
    fakeRelease = "-1.el7.noarch"
    convert :: Text -> Text
    convert name = fromMaybe ("python3-" <> name) (lookup name pipList)

-- | Install zuul in a container using software factory rpm
getRpmList :: IO [String]
getRpmList =
  do
    isContainer "sf" `bunless` cmd_ "podman" ["create", "--name", "sf", "registry.centos.org/centos:7", "sleep", "Inf"]
    isRunning "sf" `bunless` cmd_ "podman" ["start", "sf"]
    isSfReleaseInstalled "master" `bunless` run_ ["yum", "install", "-y", masterRpm]
    ensure "zuul" ["zuul-merger", "zuul-web", "zuul-executor", "zuul-scheduler", "python3-virtualenv"]
    ensure "nodepool" ["nodepool-launcher", "nodepool-builder"]
    Data.List.sort . lines <$> run ["rpm", "-qa"]
  where
    isSfReleaseInstalled release = (== release) . fromMaybe "" <$> runMaybe ["cat", "/etc/sf-release"]
    isRunning name = containerRunning . containerState . fromJust <$> inspectContainer name
    ensure name req = doesExistInContainer name `bunless` run_ (["yum", "install", "-y"] <> req)
    doesExistInContainer name = isJust <$> runMaybe ["test", "-f", "/usr/bin/" <> name]
    runContainer c arg = c "podman" (["exec", "sf"] <> arg)
    runMaybe = runContainer cmdMaybe
    run_ = runContainer cmd_
    run = runContainer cmd

masterRpm :: String
masterRpm = "https://softwarefactory-project.io/kojifiles/repos/sf-master-el7/Mash/sf-release-9999.14.g1439b64-14.el7.noarch.rpm"

comparePipAndRpm :: FilePath -> IO ()
comparePipAndRpm outputFile =
  do
    -- pip freeze output of a zuul and nodepool venv
    pipVer <- cacheAndGet "pip.txt" getPipList
    -- rpm -qa output of an installation in sf-master
    void $ cacheAndGet "rpm.txt" getRpmList
    writeFile "pipRpm.txt" $ unlines $ mapMaybe convertPipFreezeToRpmQa pipVer
    runDiff "missing.txt" "Missing package:" "--new"
    runDiff (encodeString outputFile) "Outdated packages:" "--updated"
  where
    runDiff fn desc flag = do
      putStrLn desc
      output <- cmd "pkgtreediff" ["--ignore-arch", "--ignore-release", flag, "./rpm.txt", "./pipRpm.txt"]
      writeFile fn output
      putStrLn output
      putStrLn ""
    cacheAndGet :: String -> IO [String] -> IO [String]
    cacheAndGet fn action = doesFileExist fn >>= \case
      True -> (lines <$> readFile fn)
      False -> action >>= \content -> writeFile fn (unlines content) >> return content
