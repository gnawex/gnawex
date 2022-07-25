-- Revert gnawex:citext from pg

BEGIN;

DROP EXTENSION citext;

COMMIT;
