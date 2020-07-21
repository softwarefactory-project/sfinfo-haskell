module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Gerrit
import qualified Sfinfo
import Sfinfo (comparePipAndRpm, proposeUpdate)
import Turtle ((<|>), FilePath, Parser, argPath, argText, need, options, subcommand)
import Prelude hiding (FilePath)

data Command = GetReviewStatus FilePath | ComputeDiff FilePath | ProposeUpdate FilePath Text
  deriving (Show)

-- See https://hackage.haskell.org/package/turtle-1.5.18/docs/Turtle-Options.html
usage :: Parser Command
usage = computeDiffUsage <|> proposeUpdateUsage <|> getReviewStatusUsage
  where
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
            <*> argText "gerrit-user" "Gerrit ssh name user to push review"
        )
    computeDiffUsage =
      subcommand
        "compute-diff"
        "Compare package between rpm and pypi"
        ( ComputeDiff
            <$> argPath "outdated-list" "the output file name"
        )

main :: IO ()
main = do
  command <- options "SFInfo toolkit" usage
  home' <- need "HOME"
  case command of
    GetReviewStatus sfinfoFile -> do
      sfInfo <- Sfinfo.readSFInfoFile sfinfoFile
      Gerrit.withClient "https://softwarefactory-project.io/r/" Nothing $ \gerritClient ->
        Sfinfo.getReviewStatus gerritClient sfInfo
    ComputeDiff outputFile ->
      comparePipAndRpm outputFile
    ProposeUpdate outdatedList gerritUser ->
      proposeUpdate (fromMaybe "/home/fedora" home') gerritUser "sf-master" outdatedList
