-- Revert gnawex:role_permissions from pg

BEGIN;

ALTER DEFAULT PRIVILEGES GRANT EXECUTE ON functions TO public;
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api GRANT EXECUTE ON functions TO public;

REVOKE anon, verified_user, banned_user FROM authenticator;
REVOKE USAGE ON SCHEMA app FROM auth, api, anon;

--------------------------------------------------------------------------------
-- app.users table

REVOKE REFERENCES, SELECT (user_id, username, password)
  ON TABLE app.users
  FROM auth;

REVOKE
    SELECT (user_id, username),
    INSERT (username, password),
    UPDATE (username, password)
  ON TABLE app.users
  FROM api;

REVOKE ALL ON app.users_user_id_seq FROM api;

--------------------------------------------------------------------------------
-- app.items table

REVOKE
    SELECT,
    INSERT (name, description),
    UPDATE (name, description),
    DELETE
  ON TABLE app.items
  FROM api;

REVOKE SELECT ON TABLE app.items FROM anon;
REVOKE ALL ON TABLE app.items_item_id_seq FROM verified_user, api;

--------------------------------------------------------------------------------

COMMIT;
