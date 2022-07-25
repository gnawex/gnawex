-- Deploy gnawex:authorization_schema to pg
-- requires: roles

BEGIN;

--------------------------------------------------------------------------------

CREATE SCHEMA auth authorization auth;
COMMENT ON SCHEMA auth IS
  'Schema that handles sessions and authentication';

GRANT USAGE ON SCHEMA auth TO api, anon, verified_user;
GRANT USAGE ON SCHEMA app TO auth, api, anon;

--------------------------------------------------------------------------------

COMMIT;

