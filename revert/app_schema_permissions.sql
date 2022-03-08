-- Revert gnawex:role_permissions from pg

BEGIN;

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

REVOKE SELECT ON app.transactions FROM api;

DROP POLICY authenticated_user_read_users ON app.users;
DROP POLICY authenticated_user_update_users ON app.users;
DROP POLICY auth_read_users ON app.users;
DROP POLICY api_insert_users ON app.users;

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
-- Helper functions

REVOKE EXECUTE ON FUNCTION app.current_user_id FROM api, verified_user, banned_user;
DROP FUNCTION app.current_user_id();

COMMIT;
