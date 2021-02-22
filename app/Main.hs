module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Gerrit
import Sfinfo (approveReview, comparePipAndRpm, proposeExecutorAnsibleUpdate, proposeUpdate)
import qualified Sfinfo
import Turtle (FilePath, Parser, argPath, argText, need, options, subcommand, (<|>))
import Prelude hiding (FilePath)

data Command
  = PackageWithoutReview FilePath
  | GetReviewStatus FilePath
  | ComputeDiff FilePath
  | ApproveReview FilePath Text
  | ProposeUpdate FilePath Text
  | ProposeExecutorUpdate FilePath Text
  deriving (Show)

-- See https://hackage.haskell.org/package/turtle-1.5.18/docs/Turtle-Options.html
usage :: Parser Command
usage =
  computeDiffUsage
    <|> proposeUpdateUsage
    <|> proposeExecutorUpdateUsage
    <|> approveReviewUsage
    <|> getReviewStatusUsage
    <|> getPackageWithoutReview
  where
    getPackageWithoutReview =
      subcommand
        "get-package-without-review"
        "Display the list of packages without review"
        ( PackageWithoutReview
            <$> argPath "sfinfo-file" "The sfinfo file"
        )
    getReviewStatusUsage =
      subcommand
        "get-review-status"
        "Display the status of the sfinfo packages review"
        ( GetReviewStatus
            <$> argPath "sfinfo-file" "The sfinfo file"
        )
    proposeUpdateUsage =
      subcommand
        "propose-update"
        "Generate git reviews to bump outdated packages"
        ( ProposeUpdate
            <$> argPath "outdated-list" "gen-diff output file name"
            <*> argText "gerrit-user" "Gerrit ssh name user to push review (and set GERRIT_PASSWORD environment variable for approval)"
        )
    approveReviewUsage =
      subcommand
        "approve-reviews"
        "Approve review when the gate is quiet"
        ( ApproveReview
            <$> argPath "review-list" "A list of review"
            <*> argText "gerrit-user" "Gerrit ssh name user to push review (and set GERRIT_PASSWORD environment variable for approval)"
        )
    computeDiffUsage =
      subcommand
        "compute-diff"
        "Compare package between rpm and pypi"
        ( ComputeDiff
            <$> argPath "outdated-list" "the output file name"
        )
    proposeExecutorUpdateUsage =
      subcommand
        "propose-executor-ansible-update"
        "Update executor ansible version from pypi"
        ( ProposeExecutorUpdate
            <$> argPath "sfinfo-file" "The sfinfo file"
            <*> argText "gerrit-user" "Gerrit ssh name user to push review"
        )

main :: IO ()
main =
  do
    command <- options "SFInfo toolkit" usage
    home' <- need "HOME"
    case command of
      PackageWithoutReview sfinfoFile -> sfinfoProcess sfinfoFile Sfinfo.printPackagesWithoutOpenReview
      GetReviewStatus sfinfoFile -> sfinfoProcess sfinfoFile Sfinfo.getReviewStatus
      ComputeDiff outputFile ->
        comparePipAndRpm outputFile
      ApproveReview reviewList gerritUser ->
        approveReview gerritUser reviewList
      ProposeUpdate outdatedList gerritUser ->
        proposeUpdate (fromMaybe "/home/fedora" home') gerritUser "sf-master" outdatedList
      ProposeExecutorUpdate sfinfoFile gerritUser ->
        proposeExecutorAnsibleUpdate (fromMaybe "/home/fedora" home') gerritUser sfinfoFile
  where
    sfinfoProcess sfinfo fun = do
      sfInfo <- Sfinfo.readSFInfoFile sfinfo
      Gerrit.withClient "https://softwarefactory-project.io/r/" Nothing $ \gerritClient ->
        fun gerritClient sfInfo
