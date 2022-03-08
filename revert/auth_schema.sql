-- Revert gnawex:authorization_schema from pg

BEGIN;

DROP SCHEMA auth CASCADE;

COMMIT;
