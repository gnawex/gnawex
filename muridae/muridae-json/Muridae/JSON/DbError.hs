module Muridae.JSON.DbError (DbError (..)) where

import Data.Aeson (ToJSON (toJSON), Value (String), object)
import Muridae.DB (UsageError)
import Servant.API (HasStatus (StatusOf))

newtype DbError = DbError UsageError
  deriving stock (Eq, Show)

instance HasStatus DbError where
  type StatusOf DbError = 500

instance ToJSON DbError where
  toJSON :: DbError -> Value
  toJSON _usageError =
    object
      [ ("message", String "GNAWEX ran into a problem while communicating with its database.")
      , ("help", String "There most likely isn't anything you can do. You could let the admin know.")
      ]