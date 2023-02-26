{-# LANGUAGE QuasiQuotes #-}

module Muridae.DB.ItemListing (index, create, update) where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, Effect, (:>))
import Effectful.Hasql.Pool (DB)
import Effectful.Hasql.Pool qualified as Pool
import Hasql.Decoders
  ( bool
  , column
  , int2
  , int4
  , int8
  , nonNullable
  , nullable
  , rowMaybe
  , text
  , timestamptz
  )
import Hasql.DynamicStatements.Snippet (Snippet)
import Hasql.DynamicStatements.Snippet qualified as Snippet
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Pool (UsageError)
import Hasql.TH (resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Session

-- TODO: ID, filter listing type, sort by (asc, desc): individual cost, current
-- quantity, batched by
index
  :: forall (es :: [Effect])
   . (DB :> es)
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

create
  :: forall (es :: [Effect])
   . (DB :> es)
  => Int64
  -> Int64
  -> Text
  -> Int16
  -> Int32
  -> Int32
  -> Eff
      es
      ( Either
          UsageError
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
create userId itemId listingType batchedBy unitQuantity cost =
  Pool.use . Session.transaction Session.ReadCommitted Session.Write $ do
    Transaction.statement
      userId
      [resultlessStatement|
        SELECT set_config('auth.user_id', ($1 :: BIGINT) :: TEXT, true) :: TEXT
      |]

    Transaction.statement
      (userId, itemId, listingType, batchedBy, unitQuantity, cost)
      query
 where
  query =
    [singletonStatement|
      WITH insert_listing AS (
        INSERT INTO
          app.tradable_item_listings
            ( tradable_item__id
            , user__id
            , type
            , batched_by
            , unit_quantity
            , current_unit_quantity
            , cost
            , active
            )
          VALUES
            ( $1 :: BIGINT
            , $2 :: BIGINT
            , cast($3 :: TEXT AS app.LISTING_TYPE)
            , $4 :: SMALLINT
            , $5 :: INTEGER
            , $5 :: INTEGER
            , $6 :: INTEGER
            , true
            )
          RETURNING
            tradable_item_listings.id :: BIGINT
            , tradable_item__id :: BIGINT
            , user__id :: BIGINT
            , type :: TEXT
            , batched_by :: SMALLINT
            , unit_quantity :: INTEGER
            , current_unit_quantity :: INTEGER
            , cost :: INTEGER
            , active :: BOOLEAN
            , tradable_item_listings.created_at :: TIMESTAMPTZ
            , tradable_item_listings.updated_at :: TIMESTAMPTZ?
      ) SELECT
           insert_listing.id :: BIGINT
          , tradable_item__id :: BIGINT
          , user__id :: BIGINT
          , users.username :: TEXT as username
          , type :: TEXT
          , batched_by :: SMALLINT
          , unit_quantity :: INTEGER
          , current_unit_quantity :: INTEGER
          , cost :: INTEGER
          , active :: BOOLEAN
          , insert_listing.created_at :: TIMESTAMPTZ
          , insert_listing.updated_at :: TIMESTAMPTZ?
        FROM insert_listing
        LEFT JOIN app.users
        ON insert_listing.user__id = users.id
      |]

update
  :: DB :> es
  => Int64
  -> Int64
  -> Maybe Int32
  -> Maybe Bool
  -> Eff
      es
      ( Either
          UsageError
          ( Maybe
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
update userId listingId unitQuantity active =
  Pool.use . Session.transaction Session.Serializable Session.Write $ do
    Transaction.statement
      userId
      [resultlessStatement|
        SELECT set_config('auth.user_id', ($1 :: BIGINT) :: TEXT, true) :: TEXT
      |]

    Transaction.statement () statement
 where
  statement = dynamicallyParameterized (updateQuery' unitQuantity active) decoder True

  decoder =
    rowMaybe
      ( (,,,,,,,,,,,)
          <$> column (nonNullable int8)
          <*> column (nonNullable int8)
          <*> column (nonNullable int8)
          <*> column (nonNullable text)
          <*> column (nonNullable text)
          <*> column (nonNullable int2)
          <*> column (nonNullable int4)
          <*> column (nonNullable int4)
          <*> column (nonNullable int4)
          <*> column (nonNullable bool)
          <*> column (nonNullable timestamptz)
          <*> column (nullable timestamptz)
      )

  -- TODO: Complete query
  updateQuery' :: Maybe Int32 -> Maybe Bool -> Snippet
  updateQuery' unitQuantity' active' =
    -- NOTE: What in tarnation
    mconcat
      [ "WITH update_listings AS ("
      , "UPDATE app.tradable_item_listings SET"
      , maybe
          " unit_quantity = unit_quantity"
          (mappend " unit_quantity = " . Snippet.param @Int32)
          unitQuantity'
      , maybe
          ", active = active"
          (mappend ", active = " . Snippet.param @Bool)
          active'
      , " WHERE "
      , " id = " <> Snippet.param listingId
      , " AND user__id = " <> Snippet.param userId
      , " RETURNING "
      , "  tradable_item_listings.id"
      , ", tradable_item__id"
      , ", user__id"
      , ", type"
      , ", batched_by"
      , ", unit_quantity"
      , ", current_unit_quantity"
      , ", cost"
      , ", active"
      , ", tradable_item_listings.created_at"
      , ", tradable_item_listings.updated_at"
      , ") SELECT "
      , "  update_listings.id"
      , ", tradable_item__id"
      , ", user__id"
      , ", users.username"
      , ", type"
      , ", batched_by"
      , ", unit_quantity"
      , ", current_unit_quantity"
      , ", cost"
      , ", active"
      , ", update_listings.created_at"
      , ", update_listings.updated_at"
      , " FROM update_listings"
      , " LEFT JOIN app.users"
      , " ON update_listings.user__id = users.id"
      ]
