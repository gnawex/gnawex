{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Muridae.DB.Types where

import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (Database, FromBackendRow, TableEntity)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema (Beamable, Columnar, Table (PrimaryKey, primaryKey))
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Types

data MuridaeDB f = MuridaeDB {muridaeTradableItems :: f (TableEntity TradableItem)}
  deriving stock (Generic)
  deriving anyclass (Database Postgres)

data TradableItem f = TradableItem
  { _id :: Columnar f TradableItemId
  , _name :: Columnar f Text
  , _description :: Columnar f Text
  , _wiki_link :: Columnar f Text
  , _created_at :: Columnar f UTCTime
  , _updated_at :: Columnar f (Maybe UTCTime)
  , _deleted_at :: Columnar f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

newtype TradableItemId = TradableItemId Int32
  deriving stock (Generic)
  deriving (FromBackendRow Postgres) via Int32

-------------------------------------------------------------------------------
-- Instances

instance Table TradableItem where
  newtype PrimaryKey TradableItem f = TradableItemIdPk (Columnar f TradableItemId)
    deriving stock (Generic)

  primaryKey ::
    forall (column :: Type -> Type).
    TradableItem column ->
    PrimaryKey TradableItem column
  primaryKey = TradableItemIdPk . _id

instance Beamable (PrimaryKey TradableItem)
