{-# LANGUAGE QuasiQuotes #-}

module Muridae.DB.ItemListing (index) where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, (:>))
import Effectful.Hasql.Pool (DB)
import Effectful.Hasql.Pool qualified as Pool
import Hasql.Pool (UsageError)
import Hasql.TH (vectorStatement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Session

-- TODO: ID, filter listing type, sort by (asc, desc): individual cost, current
-- quantity, batched by
index
  :: (DB :> es)
  => Eff
      es
      ( Either
          UsageError
          ( Vector
              ( Int64
              , Int64
              , Int64
              , Text
              , Text
              , Int16
              , Int32
              , Int32
              , Int32
              , Bool
              , UTCTime
              , Maybe UTCTime
              )
          )
      )
index =
  Pool.use
    . Session.transaction Session.ReadCommitted Session.Read
    . Transaction.statement ()
    $ query
 where
  query =
    [vectorStatement|
        SELECT listings.id :: BIGINT
             , tradable_item__id :: BIGINT
             , user__id :: BIGINT
             , users.username :: TEXT as username
             , type :: TEXT
             , batched_by :: SMALLINT
             , unit_quantity :: INTEGER
             , current_unit_quantity :: INTEGER
             , cost :: INTEGER
             , active :: BOOLEAN
             , listings.created_at :: TIMESTAMPTZ
             , listings.updated_at :: TIMESTAMPTZ?
          FROM app.tradable_item_listings AS listings
          JOIN app.users
          ON users.id = user__id
      |]
