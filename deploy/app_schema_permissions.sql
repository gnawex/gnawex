-- Deploy gnawex:role_permissions to pg

BEGIN;

GRANT USAGE ON SCHEMA app TO auth, api, anon;

-- Removes default privileges to execute functions
ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON functions FROM public;

-- Like the previous, except for the roles `auth` and `api`
ALTER DEFAULT PRIVILEGES FOR ROLE auth, api REVOKE EXECUTE ON functions FROM public;

--------------------------------------------------------------------------------
-- Helper functions

CREATE FUNCTION app.current_user_id()
  RETURNS INTEGER
  LANGUAGE SQL
  AS $$
    SELECT NULLIF(current_setting('auth.user_id', TRUE), '') :: INTEGER
  $$;

COMMENT ON FUNCTION app.current_user_id() IS
  'Is either NULL (if unauthenticated), or an integer (user_id of authenticated user)';

GRANT EXECUTE ON FUNCTION app.current_user_id TO api, verified_user, banned_user;

--------------------------------------------------------------------------------
-- app.users table

GRANT REFERENCES, SELECT (user_id, username, password)
  ON TABLE app.users
  TO auth;

GRANT
    SELECT (user_id, username, hunter_id, role),
    INSERT (username, password, hunter_id, role),
    UPDATE (username, password)
  ON TABLE app.users
  TO api;

GRANT ALL ON app.users_user_id_seq TO api;

-- Users can read other user data
-- TODO: Is this too broad?
CREATE POLICY authenticated_user_read_users
  ON app.users
  FOR SELECT
  USING (current_setting('role') = 'verified_user');

-- Users can update their own data
CREATE POLICY authenticated_user_update_users
  ON app.users
  FOR UPDATE
  USING (
    current_setting('role') = 'verified_user' AND
    user_id = app.current_user_id()
  )
  WITH CHECK (user_id = app.current_user_id());

-- auth should be able to select so that it can validate credentials
CREATE POLICY auth_read_users
  ON app.users
  FOR SELECT
  TO auth
  USING (true);

-- API should be able to register users
CREATE POLICY api_insert_users
  ON app.users
  FOR INSERT
  TO api
  WITH CHECK (true);

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
-- app.transactions

GRANT SELECT
  ON TABLE app.transactions
  TO api;

--------------------------------------------------------------------------------

COMMIT;
