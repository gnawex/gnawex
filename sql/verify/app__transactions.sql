-- Verify gnawex:transactions on pg

BEGIN;

SELECT
    id
  , buy_item_listing__id
  , sell_item_listing__id
  , quantity
  , status
  , buyer__id
  , seller__id
  , created_at
  , updated_at
  FROM app.tradable_item_transactions
  WHERE false;

ROLLBACK;
