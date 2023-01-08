module Muridae.User.Types where

import Control.Monad.Identity (Identity)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Beam (
    Beamable,
    Columnar,
    FromBackendRow,
    HasSqlEqualityCheck,
    Table (PrimaryKey, primaryKey),
 )
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Postgres (
    Postgres,
    ResultError (ConversionFailed, Incompatible, UnexpectedNull),
 )
import Database.Beam.Postgres.CustomTypes (pgEnumValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.PostgreSQL.Simple.FromField (
    FromField (fromField),
    returnError,
    typename,
 )
import GHC.Generics (Generic)

data Role = Verified | Unverified | Banned
    deriving stock (Generic, Show)

data User f = User
    { _id :: Columnar f UserId
    , _username :: Columnar f Text
    , _hunter_id :: Columnar f Int32
    , _password :: Columnar f Text
    , _role :: Columnar f Role
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

newtype UserId = UserId Int32
    deriving stock (Generic, Show)
    deriving
        ( FromBackendRow Postgres
        , HasSqlValueSyntax PgValueSyntax
        , HasSqlEqualityCheck Postgres
        )
        via Int32

deriving instance Show (User Identity)

deriving instance Show (PrimaryKey User Identity)

instance Table User where
    newtype PrimaryKey User f
        = UserPk (Columnar f UserId)
        deriving stock (Generic)

    primaryKey
        :: forall (column :: Type -> Type)
         . User column
        -> PrimaryKey User column
    primaryKey item = UserPk (item._id)

instance Beamable (PrimaryKey User)

instance FromBackendRow Postgres Role

instance FromField Role where
    fromField f mbValue = do
        fieldType <- typename f
        case fieldType of
            "role" -> do
                case mbValue of
                    Nothing ->
                        returnError UnexpectedNull f ""
                    Just value ->
                        case value of
                            "verified_user" ->
                                pure Verified
                            "unverified_user" ->
                                pure Unverified
                            "banned_user" -> pure Banned
                            _ ->
                                returnError ConversionFailed f "Could not 'read' value for 'ListingType'"
            _ ->
                returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax Role where
    sqlValueSyntax = pgEnumValueSyntax $ \case
        Verified -> "verified_user"
        Unverified -> "unverified_user"
        Banned -> "banned_user"
