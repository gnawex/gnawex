module MuridaeWeb.JSON.DbError (DbError (..)) where

--------------------------------------------------------------------------------

import Muridae.DB (UsageError)
import Data.Aeson (Value (String), ToJSON (toJSON), object)
import Servant (HasStatus (StatusOf))

--------------------------------------------------------------------------------

-- | Wrapped version of `UsageError`. Use this when serializing to JSON.
newtype DbError = DbError UsageError
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------

instance ToJSON DbError where
  toJSON :: DbError -> Value
  toJSON (DbError _e) =
    object
      [ ("message", String "GNAWEX was unable to establish a connection with the database.")
      , ("help", String "If you are the GNAWEX admin, ensure that the DB credentials are valid.")
      ]

instance HasStatus DbError where
  type StatusOf DbError = 500
