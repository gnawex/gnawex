-- Revert gnawex:pgcrypto from pg

BEGIN;

DROP EXTENSION pgcrypto;

COMMIT;
