module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Sfinfo.Data
import Sfinfo.RpmSpec
import Sfinfo.Yaml
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  beforeSpec <- T.readFile "./test/data/python-APScheduler.spec.original"
  afterSpec <- T.readFile "./test/data/python-APScheduler.spec.updated"
  beforeYaml <- T.readFile "./test/data/resources.yaml.original"
  afterYaml <- T.readFile "./test/data/resources.yaml.updated"
  defaultMain (tests (beforeSpec, afterSpec) (beforeYaml, afterYaml))

tests :: (Text, Text) -> (Text, Text) -> TestTree
tests dataFiles yamlFiles =
  testGroup
    "Tests"
    [ unitTests dataFiles,
      yamlTests yamlFiles
    ]

encode :: Text -> BSL.ByteString
encode = BSL.fromStrict . T.encodeUtf8

decode :: BSL.ByteString -> Text
decode = T.decodeUtf8 . BSL.toStrict

yamlTests :: (Text, Text) -> TestTree
yamlTests (beforeYaml, afterYaml) =
  testGroup
    "yaml"
    [ testCase "yamlfile updated" $
        assertEqual
          "Comment are preserved"
          afterYaml
          (decode $ transformYaml addReleaseBranch (encode beforeYaml))
    ]
  where
    addReleaseBranch :: Maybe Text -> SFRepo -> SFRepo
    addReleaseBranch (Just "rpms/zuul") repo = addBranch "3.5" "master" repo
    addReleaseBranch _ repo = repo

unitTests :: (Text, Text) -> TestTree
unitTests (beforeSpec, afterSpec) =
  testGroup
    "unitTests"
    [ testCase "specfile updated" $
        assertEqual
          "Specfile is updated"
          afterSpec
          ( bumpVersion
              "4.2.0"
              "dlrn <dlrn@sf.io>"
              "Thu Jul 16 22 2020"
              beforeSpec
          )
    ]
