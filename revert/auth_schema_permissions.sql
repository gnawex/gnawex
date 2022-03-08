-- Revert gnawex:auth_schema_permissions from pg

BEGIN;

SET LOCAL ROLE auth;

REVOKE USAGE ON SCHEMA auth FROM api, anon, verified_user;

COMMIT;
