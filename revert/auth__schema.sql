-- Revert gnawex:authorization_schema from pg

BEGIN;

REVOKE USAGE ON SCHEMA auth FROM api, anon, verified_user;
REVOKE USAGE ON SCHEMA app FROM auth, api, anon;

DROP SCHEMA auth CASCADE;

COMMIT;
