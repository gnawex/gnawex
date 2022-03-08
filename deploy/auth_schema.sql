-- Deploy gnawex:authorization_schema to pg
-- requires: roles
-- requires: role_permissions

BEGIN;

CREATE SCHEMA auth authorization auth;
COMMENT ON SCHEMA auth IS
  'Schema that handles sessions and authentication';

COMMIT;

