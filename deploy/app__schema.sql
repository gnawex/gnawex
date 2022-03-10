-- Deploy gnawex:app_schema to pg

BEGIN;

CREATE SCHEMA app;
COMMENT ON SCHEMA app IS
  'State and business logic of GNAWEX';

GRANT USAGE ON SCHEMA app TO api, auth, gnawex_merchant, verified_user;

COMMIT;
