-- Verify gnawex:items on pg

BEGIN;

SELECT item_id, name, description, created_at
  FROM app.items
  WHERE false;

ROLLBACK;
