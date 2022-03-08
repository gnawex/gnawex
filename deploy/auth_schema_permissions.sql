-- Deploy gnawex:auth_schema_permissions to pg

BEGIN;

SET LOCAL ROLE auth;

GRANT USAGE ON SCHEMA auth TO api, anon, verified_user;

COMMIT;
