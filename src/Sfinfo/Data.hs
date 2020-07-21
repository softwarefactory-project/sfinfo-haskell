{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module Sfinfo.Data
  ( SFRepo (..),
    addBranch,
  )
where

import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)

data SFRepo
  = SFInfoPackage
      { acl :: Text,
        description :: Text,
        branches :: Maybe (M.Map Text Text)
      }
  deriving (Show, Generic, FromJSON)

instance ToJSON SFRepo where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

addBranch :: Text -> Text -> SFRepo -> SFRepo
addBranch branch ref repo = repo {branches = Just newBranches}
  where
    newBranches = case branches repo of
      Just existingBranches -> M.insert branch ref existingBranches
      Nothing -> M.fromList [(branch, ref)]
