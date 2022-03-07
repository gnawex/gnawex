-- Verify gnawex:listings on pg

BEGIN;

SELECT
    listing_id,
    item_id,
    user_id,
    quantity,
    cost,
    type,
    batch,
    is_active,
    created_at,
    updated_at
  FROM app.listings
  WHERE false;

ROLLBACK;
