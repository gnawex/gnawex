-- Revert gnawex:app_schema from pg

BEGIN;

REVOKE USAGE ON SCHEMA app FROM gnawex_merchant, api, auth, verified_user;

DROP SCHEMA app;

COMMIT;
