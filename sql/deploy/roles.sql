-- Deploy gnawex:roles to pg

BEGIN;

--------------------------------------------------------------------------------

-- 1. authenticator
DO $$
BEGIN
CREATE ROLE authenticator NOINHERIT LOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE authenticator IS
  'Role that serves as an entrypoint';

-- 2. anon
DO $$
BEGIN
CREATE ROLE anon NOINHERIT NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE anon IS
  'Role that PostgREST will switch to when a user is not authenticated.';

-- 3. verified_user
DO $$
BEGIN
CREATE ROLE verified_user NOINHERIT NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE verified_user IS
  'Role that PostgREST will switch to when a user is authenticated, and verified.';

-- 3. banned_user
DO $$
BEGIN
CREATE ROLE banned_user NOINHERIT NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE banned_user IS
  'Role that PostgREST will switch to when a user is authenticated, but banned.';

-- 4. auth
DO $$
BEGIN

CREATE ROLE auth NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE auth IS
  'Role that owns the `auth` schema and its objects';

-- 5. api
DO $$
BEGIN
CREATE ROLE api NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE api IS
  'Role that owns the `api` schema and its objects';

-- 6. gnawex_merchant
DO $$
BEGIN
CREATE ROLE gnawex_merchant NOLOGIN;
  EXCEPTION WHEN duplicate_object
    THEN RAISE NOTICE '%, skipping', SQLERRM USING ERRCODE = SQLSTATE;
END
$$;

COMMENT ON ROLE gnawex_merchant IS
  'Role to manage listings, and insert transactions.';

--------------------------------------------------------------------------------
-- Permissions

-- Allows `authenticator` to switch to any of the ff:
--
-- 1. `anon`
-- 2. `verified_user`
-- 3. `banned_user`
GRANT anon, verified_user, banned_user, gnawex_merchant TO authenticator;

-- Removes default privileges to execute functions
ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON functions FROM public;

-- Like the previous, except for the roles `auth` and `api`
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api REVOKE EXECUTE ON functions FROM public;

--------------------------------------------------------------------------------

COMMIT;
