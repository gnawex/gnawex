-- Revert gnawex:app_schema from pg

BEGIN;

REVOKE USAGE ON SCHEMA app FROM gnawex_merchant;

DROP SCHEMA app;

COMMIT;
