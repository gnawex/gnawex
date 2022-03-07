-- Revert gnawex:items from pg

BEGIN;

DROP TABLE app.items;

COMMIT;
