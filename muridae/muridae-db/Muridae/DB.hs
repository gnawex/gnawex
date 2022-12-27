{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Muridae.DB where

-- This is just a sample module with sample code

import qualified Control.Monad as Monad
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, liftIO)
import Database.Beam.Postgres (Pg, Postgres, Connection)
import qualified Database.Beam.Postgres as Beam
import qualified Database.Beam.Query as Query
import Database.Beam.Schema (Database, DatabaseSettings, Table (PrimaryKey, primaryKey), TableEntity)
import qualified Database.Beam.Schema as Schema
import GHC.Generics (Generic)

data User f = User
  { userEmail :: Columnar f Text
  , userId :: Columnar f Int32
  , userNumOfListings :: Columnar f Int32
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

data MarketDB f = MarketDB {marketUsers :: f (TableEntity User)}
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

deriving instance Show (User Identity)
deriving instance Show (PrimaryKey User Identity)

instance Table User where
  newtype PrimaryKey User f = UserId (Columnar f Int32)
    deriving stock (Generic)

  primaryKey ::
    forall (column :: Type -> Type).
    User column ->
    PrimaryKey User column
  primaryKey = UserId . userId

instance Beamable (PrimaryKey User)

marketDB :: DatabaseSettings Postgres MarketDB
marketDB = Schema.defaultDbSettings

example :: Connection -> IO ()
example conn =
  Beam.runBeamPostgresDebug
    print
    conn
    ( pure ()
       >> selectUsers
       >> selectSortedUsers
       >>= liftIO
         . print
       >> sumOfListings
       >>= liftIO
         . print
    )

sumOfListings :: Pg (Maybe Int32)
sumOfListings = do
  let agg =
        Query.aggregate_
          (\user -> Query.as_ @(Maybe Int32) (Query.sum_ (userNumOfListings user)))
          (Query.all_ (marketUsers marketDB))

  Query.runSelectReturningOne (Query.select agg) >>= pure . Monad.join

selectSortedUsers :: Pg [User Identity]
selectSortedUsers = do
  let sortUsersByEmail =
        Query.limit_ 1 $
          Query.offset_ 1 $
            Query.orderBy_
              (\u -> (Query.desc_ (userEmail u)))
              (Query.all_ (marketUsers marketDB))

  Query.runSelectReturningList (Query.select sortUsersByEmail)

populateUsers :: Pg ()
populateUsers = do
  Query.runInsert $
    Query.insert (marketUsers marketDB) $
      Query.insertExpressions
        [ User "foo1@bar.com" Query.default_ Query.default_
        , User "foo2@bar.com" Query.default_ 1
        , User "foo3@bar.com" Query.default_ 20
        ]

selectUsers :: Pg [User Identity]
selectUsers = do
  Query.runSelectReturningList (Query.select (Query.all_ (marketUsers marketDB)))
