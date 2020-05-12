module OpenApi.MediaType where

import OpenApi.JSON
import OpenApi.Prelude
import OpenApi.Reference
import OpenApi.Schema

import qualified Data.Aeson as JSON

newtype MediaType = MediaType
  { schema :: ReferenceOr Schema }
  deriving anyclass JSON.ToJSON
  deriving stock    (Eq, Generic, Show)

instance JSON.FromJSON MediaType where
  parseJSON = genericParseJSON