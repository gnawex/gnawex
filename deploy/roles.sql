-- Deploy gnawex:roles to pg

BEGIN;

-- 1. authenticator
CREATE ROLE authenticator NOINHERIT LOGIN;
COMMENT ON ROLE authenticator IS
  'Role that serves as an entrypoint';

-- 2. anon
CREATE ROLE anon NOINHERIT NOLOGIN;
COMMENT ON ROLE anon IS
  'Role that PostgREST will switch to when a user is not authenticated.';

-- 3. verified_user
CREATE ROLE verified_user NOINHERIT NOLOGIN;
COMMENT ON ROLE verified_user IS
  'Role that PostgREST will switch to when a user is authenticated, and verified.';

-- 3. banned_user
CREATE ROLE banned_user NOINHERIT NOLOGIN;
COMMENT ON ROLE banned_user IS
  'Role that PostgREST will switch to when a user is authenticated, but banned.';

-- 4. auth
CREATE ROLE auth NOLOGIN;
COMMENT ON ROLE auth IS
  'Role that owns the `auth` schema and its objects';

-- 5. api
CREATE ROLE api NOLOGIN;
COMMENT ON ROLE api IS
  'Role that owns the `api` schema and its objects';

-- Allows `authenticator` to switch to any of the ff:
--
-- 1. `anon`
-- 2. `verified_user`
-- 3. `banned_user`
GRANT anon, verified_user, banned_user TO authenticator;

COMMIT;
