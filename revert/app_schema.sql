-- Revert gnawex:app_schema from pg

BEGIN;

DROP SCHEMA app;

COMMIT;
