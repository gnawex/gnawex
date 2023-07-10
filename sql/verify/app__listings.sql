-- Verify gnawex:listings on pg

BEGIN;

SELECT
    id
  , tradable_item__id
  , user__id
  , unit_quantity
  , current_unit_quantity
  , cost
  , type
  , batched_by
  , active
  , created_at
  , updated_at
  FROM app.tradable_item_listings
  WHERE false;

ROLLBACK;