-- Revert gnawex:pgtap from pg

BEGIN;

DROP EXTENSION pgtap;

COMMIT;
