{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Sfinfo
  ( proposeUpdate,
    comparePipAndRpm,
    readPkgTreeDiffOutputFile,
    readSFInfoFile,
    getReviewStatus,
    printPackagesWithoutOpenReview,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
import qualified Data.ByteString
import Data.Either (lefts, rights)
import qualified Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Yaml
import Distribution.RPM.PackageTreeDiff (Ignore (..), RpmPackage (..), RpmPackageDiff (..), diffPkgs, readRpmPkg, rpmPkgVerRel)
import GHC.Generics (Generic)
import Gerrit (GerritClient)
import qualified Gerrit
import Gerrit.Data (GerritChange)
import Podman (containerRunning, containerState, inspectContainer, isContainer)
import Sfinfo.Cloner (clone, commit, gitReview, mkChangeId, reset, urlToGitDir)
import Sfinfo.PipNames (ignoreList, pipList)
import Sfinfo.RpmSpec (bumpVersion, getDate, getSpec)
import SimpleCmd (cmd, cmdMaybe, cmd_)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.PrettyPrint.ANSI.Leijen (green, putDoc, text)
import Turtle (FilePath, encodeString)
import Zuul (ZuulClient)
import qualified Zuul
import qualified Zuul.Status as Zuul
import Prelude hiding (FilePath, id)

data SFInfoPackage
  = SFInfoPackage
      { name :: Text,
        source :: Text,
        spec :: Maybe Text
      }
  deriving (Show, Generic, FromJSON)

data SFInfo
  = SFInfo
      { branch :: Text,
        packages :: [SFInfoPackage]
      }
  deriving (Show, Generic, FromJSON)

getOpenReviews :: GerritClient -> [SFInfoPackage] -> IO [Maybe [GerritChange]]
getOpenReviews gerritClient = mapM go
  where
    go :: SFInfoPackage -> IO (Maybe [GerritChange])
    go SFInfoPackage {..} = do
      gerritChanges <- Gerrit.queryChanges [Gerrit.Project name, Gerrit.Status Gerrit.NEW] gerritClient
      pure $ case gerritChanges of
        [] -> Nothing
        xs -> Just xs

getPackagesWithoutOpenReviews :: GerritClient -> [SFInfoPackage] -> IO [SFInfoPackage]
getPackagesWithoutOpenReviews gerritClient packages = do
  openReviews <- getOpenReviews gerritClient packages
  pure $ map fst $ filter (\(_, reviews) -> isNothing reviews) (zip packages openReviews)

printPackagesWithoutOpenReview :: GerritClient -> SFInfo -> IO ()
printPackagesWithoutOpenReview gerritClient sfinfo =
  do
    packagesWithoutOpenReview <- getPackagesWithoutOpenReviews gerritClient (packages sfinfo)
    forM_ packagesWithoutOpenReview (putStrLn . T.unpack . name)

getReviewStatus :: GerritClient -> SFInfo -> IO ()
getReviewStatus gerritClient sfinfo =
  do
    openReviews <- getOpenReviews gerritClient packagesList
    mapM_ go (zip packagesList openReviews)
  where
    packagesList = packages sfinfo
    go :: (SFInfoPackage, Maybe [GerritChange]) -> IO ()
    go (SFInfoPackage {..}, Just gerritChanges) = putStrLn $ T.unpack $ name <> ": " <> changeUrls gerritChanges
    go _ = pure ()
    changeUrls :: [Gerrit.GerritChange] -> Text
    changeUrls changes = T.unwords $ map changeUrl changes
    changeUrl :: Gerrit.GerritChange -> Text
    changeUrl = Gerrit.changeUrl gerritClient

readSFInfoFile :: FilePath -> IO SFInfo
readSFInfoFile fn = do
  fContent <- Data.ByteString.readFile (encodeString fn)
  case Data.Yaml.decodeEither' fContent of
    Right fObj -> pure fObj
    Left err -> error $ "Invalid sfinfo file: " <> encodeString fn <> " " <> show err

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

--- | Creat the bump version commit
commitUpdate :: Text -> Text -> Text -> Text -> IO ()
commitUpdate gitDir changeId packageVersion commitTitle = do
  reset gitDir "origin/master"
  (specPath, specContent) <- getSpec gitDir
  date <- getDate
  T.writeFile specPath (bumpVersion packageVersion author date specContent)
  commit gitDir commitTitle changeId

approveChange :: GerritClient -> Gerrit.GerritChange -> IO ()
approveChange client change =
  do
    putStrLn $ T.unpack $ "Approving " <> Gerrit.project change <> " (" <> Gerrit.subject change <> ")"
    -- TODO: get approver name
    void $ Gerrit.postReview change "Thanks!" "Workflow" 1 client

queueLength :: Text -> Text -> Zuul.Status -> Int
queueLength pipelineName queueName = length . fromJust . Zuul.pipelineChanges pipelineName (Just queueName)

getQueueLength :: ZuulClient -> Text -> IO Int
getQueueLength zuulClient queueName = queueLength "gate" queueName <$> Zuul.getStatus zuulClient

projectName :: Text -> Text
projectName packageName = "rpms/" <> T.replace "python3-" "python-" packageName

-- | This function returns either an optional (package, message) tuple, either a GerritChange to approve
updatePackage :: GerritClient -> Text -> Text -> Text -> Text -> IO (Either (Maybe (Text, Text)) Gerrit.GerritChange)
updatePackage gerritClient gerritUser home packageName packageVersion =
  do
    gerritChanges <- Gerrit.queryChanges [Gerrit.Project project', Gerrit.ChangeId changeId] gerritClient
    case gerritChanges of
      [] -> proposeReview
      [change@Gerrit.GerritChange {..}]
        | status == Gerrit.MERGED -> skipResult
        -- TODO: if review parent is identic to local git parent then rebase and push
        | Gerrit.hasLabel "Workflow" 1 change -> infoResult "already approved"
        | Gerrit.hasLabel "Code-Review" 2 change -> addWorkflow change
        | otherwise -> infoResult $ changeUrl change <> " : need Approval"
      _ -> infoResult $ T.pack $ "multiple review opened: " <> show (map changeUrl gerritChanges)
  where
    -- TODO: abandon old change when multiple review exist?
    proposeReview = do
      void $ clone gitBase projectUrl
      commitUpdate gitDir changeId packageVersion commitTitle
      gitReview gerritUser project' gitDir
      infoResult "review submitted"
    addWorkflow = pure . Right
    skipResult = pure $ Left Nothing
    infoResult txt = pure $ Left (Just (packageName, txt))
    project' = projectName packageName
    changeUrl = Gerrit.changeUrl gerritClient
    gitBase = home <> "/src/"
    projectUrl = "https://softwarefactory-project.io/r/" <> project'
    gitDir = urlToGitDir gitBase projectUrl
    changeId = mkChangeId gitDir commitTitle
    commitTitle = "Bump to " <> packageVersion

-- | This is the main CLI action that propose update based on the pkgtreediff output file
proposeUpdate :: Text -> Text -> Text -> FilePath -> IO ()
proposeUpdate home gerritUser zuulQueueName pkgtreediffFile =
  Gerrit.withClient "https://softwarefactory-project.io/r" (Just gerritUser) $ \gerritClient ->
    Zuul.withClient "https://softwarefactory-project.io/zuul/api/tenant/local" $ \zuulClient ->
      go gerritClient zuulClient
  where
    maxGateLength = 4
    getUpdates gerritClient = mapM (uncurry (updatePackage gerritClient gerritUser home))
    go :: GerritClient -> ZuulClient -> IO ()
    go gerritClient zuulClient = do
      pkgsVers <- readPkgTreeDiffOutputFile pkgtreediffFile
      results <- getUpdates gerritClient pkgsVers
      let infos = catMaybes $ lefts results
      let toApprove = rights results
      forM_ infos $ \(packageName, info) -> putStrLn $ T.unpack $ packageName <> ": " <> info
      gateLength <- getQueueLength zuulClient zuulQueueName
      if gateLength >= maxGateLength
        then putStrLn "Zuul is too busy atm, try again later"
        else
          putDoc (green (text "Approving:\n"))
            >> mapM_ (approveChange gerritClient) (take (maxGateLength - gateLength) toApprove)

readPkgTreeDiffOutputFile :: FilePath -> IO [(Text, Text)]
readPkgTreeDiffOutputFile fn = do
  fnContent <- T.lines <$> T.readFile (encodeString fn)
  pure $ map readOutdatedLine fnContent

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
    rpmVer <- cacheAndGet "rpm.txt" getRpmList
    let pipVerAsRpm = Data.List.sort $ mapMaybe convertPipFreezeToRpmQa pipVer
    writeFile "pipRpm.txt" (unlines pipVerAsRpm)
    writeFile (encodeString outputFile) (unlines $ runDiff rpmVer pipVerAsRpm)
  where
    runDiff :: [String] -> [String] -> [String]
    runDiff rpms pips = map T.unpack $ mapMaybe createDiffString $ diffPkgs IgnoreRelease (map mkPkgs rpms) (map mkPkgs pips)
    createDiffString :: RpmPackageDiff -> Maybe Text
    createDiffString (PkgUpdate rpm pip) = Just $ rpmName rpm <> ": " <> rpmPkgVerRel rpm <> " -> " <> rpmPkgVerRel pip
    createDiffString _ = Nothing
    mkPkgs :: String -> RpmPackage
    mkPkgs = dropArch . readRpmPkg . T.pack
    dropArch :: RpmPackage -> RpmPackage
    dropArch (RpmPkg n vr _) = RpmPkg n vr Nothing
    cacheAndGet :: String -> IO [String] -> IO [String]
    cacheAndGet fn action = doesFileExist fn >>= \case
      True -> (lines <$> readFile fn)
      False -> action >>= \content -> writeFile fn (unlines content) >> return content
