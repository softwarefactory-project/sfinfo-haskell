module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sfinfo.RpmSpec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  beforeSpec <- T.readFile "./test/data/python-APScheduler.spec.original"
  afterSpec <- T.readFile "./test/data/python-APScheduler.spec.updated"
  defaultMain (tests (beforeSpec, afterSpec))

tests :: (Text, Text) -> TestTree
tests dataFiles = testGroup "Tests" [unitTests dataFiles]

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
