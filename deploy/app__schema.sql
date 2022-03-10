-- Deploy gnawex:app_schema to pg

BEGIN;

CREATE SCHEMA app;
COMMENT ON SCHEMA app IS
  'State and business logic of GNAWEX';

GRANT USAGE ON SCHEMA app TO gnawex_merchant;

COMMIT;
