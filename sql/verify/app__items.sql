-- Verify gnawex:items on pg

BEGIN;

SELECT id, name, description, created_at
  FROM app.tradable_items
  WHERE false;

ROLLBACK;
