-- Revert gnawex:users from pg

BEGIN;

DROP TRIGGER cryptpassword ON app.users;
DROP FUNCTION app.cryptpassword();

-- app.users permissions
DROP POLICY api_insert_users ON app.users;
DROP POLICY auth_read_users ON app.users;
DROP POLICY authenticated_user_update_users ON app.users;
DROP POLICY authenticated_user_read_users ON app.users;

REVOKE EXECUTE ON FUNCTION app.current_user_id FROM api, verified_user, banned_user;
DROP FUNCTION app.current_user_id;

ALTER TABLE app.users DISABLE ROW LEVEL SECURITY;

REVOKE ALL ON app.users_id_seq FROM api;
REVOKE
    SELECT (id, username, hunter_id, role),
    INSERT (username, password, hunter_id, role),
    UPDATE (username, password)
  ON TABLE app.users
  FROM api;

REVOKE REFERENCES, SELECT (id, username, password)
  ON TABLE app.users
  FROM auth;

DROP TABLE app.users;
DROP TYPE app.USER_ROLE;

COMMIT;
