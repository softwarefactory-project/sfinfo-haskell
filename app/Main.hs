module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Sfinfo (comparePipAndRpm, proposeUpdate)
import Turtle ((<|>), FilePath, Parser, argPath, argText, need, options, subcommand)
import Prelude hiding (FilePath)

data Command = ComputeDiff FilePath | ProposeUpdate FilePath Text
  deriving (Show)

usage :: Parser Command
usage = computeDiffUsage <|> proposeUpdateUsage
  where
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
    ComputeDiff outputFile ->
      comparePipAndRpm outputFile
    ProposeUpdate outdatedList gerritUser ->
      proposeUpdate (fromMaybe "/home/fedora" home') gerritUser outdatedList
