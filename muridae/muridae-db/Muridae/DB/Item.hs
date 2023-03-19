{-# LANGUAGE QuasiQuotes #-}

module Muridae.DB.Item (module Muridae.DB.Item) where

--------------------------------------------------------------------------------

import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, (:>))
import Effectful.Hasql.Pool (DB)
import Effectful.Hasql.Pool qualified as Pool
import Hasql.Pool (UsageError)
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Session

--------------------------------------------------------------------------------

index
  :: (DB :> es)
  => Eff
      es
      ( Either
          UsageError
          ( Vector
              ( Int64
              , Text
              , Text
              , Text
              , UTCTime
              , Maybe UTCTime
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
      SELECT id :: BIGINT
           , name :: TEXT
           , description :: TEXT
           , wiki_link :: TEXT
           , created_at :: TIMESTAMPTZ
           , updated_at :: TIMESTAMPTZ?
           , deleted_at :: TIMESTAMPTZ?
        FROM app.tradable_items
    |]

create
  :: (DB :> es)
  => Text
  -> Text
  -> Text
  -> Eff
      es
      ( Either
          UsageError
          ( Int64
          , Text
          , Text
          , Text
          , UTCTime
          , Maybe UTCTime
          , Maybe UTCTime
          )
      )
create name desc wikiLink =
  Pool.use
    . Session.transaction Session.ReadCommitted Session.Write
    . Transaction.statement (name, desc, wikiLink)
    $ query
 where
  query =
    [singletonStatement|
      INSERT INTO app.tradable_items
        ( name
        , description
        , wiki_link
        )
        VALUES ($1 :: TEXT, $2 :: TEXT, $3 :: TEXT)
        RETURNING
           id :: BIGINT
          , name :: TEXT
          , description :: TEXT
          , wiki_link :: TEXT
          , created_at :: TIMESTAMPTZ
          , updated_at :: TIMESTAMPTZ?
          , deleted_at :: TIMESTAMPTZ?
    |]

find
  :: (DB :> es)
  => Int64
  -> Eff
      es
      ( Either
          UsageError
          ( Maybe
              ( Int64
              , Text
              , Text
              , Text
              , UTCTime
              , Maybe UTCTime
              , Maybe UTCTime
              , Vector (Int32, Int16, Int64, Scientific)
              , Vector (Int32, Int16, Int64, Scientific)
              )
          )
      )
find itemId =
  Pool.use . Session.transaction Session.ReadCommitted Session.Read $ do
    item <- Transaction.statement itemId itemQuery
    pooledBuys <- Transaction.statement itemId pooledBuyQuery
    pooledSells <- Transaction.statement itemId pooledSellQuery

    pure $ f pooledBuys pooledSells <$> item
 where
  f buys sells (itemId', name, desc, wikiLink, createdAt, updatedAt, deletedAt) =
    (itemId', name, desc, wikiLink, createdAt, updatedAt, deletedAt, buys, sells)

  itemQuery =
    [maybeStatement|
      SELECT id :: BIGINT
        , name :: TEXT
        , description :: TEXT
        , wiki_link :: TEXT
        , created_at :: TIMESTAMPTZ
        , updated_at :: TIMESTAMPTZ?
        , deleted_at :: TIMESTAMPTZ?
        FROM app.tradable_items
        WHERE id = $1 :: BIGINT
      |]

  -- TODO: Decode to pg enum
  -- TODO: Consider using dynamic statements
  pooledBuyQuery =
    [vectorStatement|
      SELECT cost :: INT
           , batched_by :: SMALLINT
           , sum(unit_quantity) :: BIGINT AS unit_quantity
           , (cast(cost AS NUMERIC) / cast(batched_by AS NUMERIC))
                :: NUMERIC AS individual_cost
        FROM app.tradable_item_listings
        WHERE type = 'buy'
          AND active = true
          AND tradable_item__id = $1 :: BIGINT
        GROUP BY batched_by, cost
        ORDER BY individual_cost DESC
        FETCH FIRST 5 ROWS ONLY
    |]

  pooledSellQuery =
    [vectorStatement|
      SELECT cost :: INT
           , batched_by :: SMALLINT
           , sum(unit_quantity) :: BIGINT AS unit_quantity
           , (cast(cost AS NUMERIC) / cast(batched_by AS NUMERIC))
                :: NUMERIC AS individual_cost
        FROM app.tradable_item_listings
        WHERE type = 'sell'
          AND active = true
          AND tradable_item__id = $1 :: BIGINT
        GROUP BY batched_by, cost
        ORDER BY individual_cost ASC
        FETCH FIRST 5 ROWS ONLY
    |]
