-- Deploy gnawex:role_permissions to pg

BEGIN;

-- Allows `authenticator` to switch to any of the ff:
--
-- 1. `anon`
-- 2. `verified_user`
-- 3. `banned_user`
GRANT anon, verified_user, banned_user TO authenticator;

GRANT USAGE ON SCHEMA app TO auth, api, anon;

-- Removes default privileges to execute functions
ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON functions FROM public;

-- Like the previous, except for the roles `auth` and `api`
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api REVOKE EXECUTE ON functions FROM public;

--------------------------------------------------------------------------------
-- app.users table

GRANT REFERENCES, SELECT (user_id, username, password)
  ON TABLE app.users
  TO auth;

GRANT
    SELECT (user_id, username),
    INSERT (username, password),
    UPDATE (username, password)
  ON TABLE app.users
  TO api;

GRANT ALL ON app.users_user_id_seq TO api;

--------------------------------------------------------------------------------
-- app.items table

GRANT
    SELECT,
    INSERT (name, description),
    UPDATE (name, description),
    DELETE
  ON TABLE app.items
  TO api;

GRANT SELECT ON TABLE app.items TO anon;
GRANT ALL ON TABLE app.items_item_id_seq TO api, verified_user;

--------------------------------------------------------------------------------

COMMIT;
