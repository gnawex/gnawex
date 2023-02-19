{-# LANGUAGE QuasiQuotes #-}

module Muridae.DB.Item (module Muridae.DB.Item) where

--------------------------------------------------------------------------------

import Data.Int (Int64)
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
           , wiki_link :: TEXT
           , description :: TEXT
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
    . Session.transaction Session.ReadCommitted Session.Read
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
              (Int64, Text, Text, Text, UTCTime, Maybe UTCTime, Maybe UTCTime)
          )
      )
find itemId =
  Pool.use
    . Session.transaction Session.ReadCommitted Session.Read
    . Transaction.statement itemId
    $ query
 where
  query =
    [maybeStatement|
        SELECT id :: BIGINT
          , name :: TEXT
          , wiki_link :: TEXT
          , description :: TEXT
          , created_at :: TIMESTAMPTZ
          , updated_at :: TIMESTAMPTZ?
          , deleted_at :: TIMESTAMPTZ?
          FROM app.tradable_items
          WHERE id = $1 :: BIGINT
      |]
