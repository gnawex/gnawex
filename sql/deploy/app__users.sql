-- Deploy gnawex:users to pg
-- requires: extension__app_schema
-- requires: extension__pgcrypto
-- requires: extension__citext

BEGIN;

--------------------------------------------------------------------------------

CREATE FUNCTION app.current_user_id()
  RETURNS INTEGER
  LANGUAGE SQL
  AS $$
    SELECT NULLIF(current_setting('auth.user_id', TRUE), '') :: INTEGER
  $$;

COMMENT ON FUNCTION app.current_user_id() IS
  'Is either NULL (if unauthenticated), or an integer (user_id of authenticated user)';

GRANT EXECUTE ON FUNCTION app.current_user_id TO api, verified_user;

--------------------------------------------------------------------------------

CREATE TYPE app.USER_ROLE AS ENUM ('verified_user', 'unverified_user', 'banned_user');
COMMENT ON TYPE app.USER_ROLE IS
  'Type of user';

--------------------------------------------------------------------------------

CREATE TABLE app.users (
  id   BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  hunter_id BIGINT UNIQUE NOT NULL,
  username  CITEXT UNIQUE NOT NULL,

  -- 72 character limit due to 'bf' hash
  password  TEXT NOT NULL
            CHECK (char_length(password) <= 72 AND char_length(password) >= 10),

  role      app.USER_ROLE NOT NULL
);

COMMENT ON TABLE app.users IS
  'GNAWEX users';

GRANT REFERENCES, SELECT (id, username, password)
  ON TABLE app.users
  TO auth;

GRANT
    SELECT (id, username, hunter_id, role),
    INSERT (username, password, hunter_id, role),
    UPDATE (username, password)
  ON TABLE app.users
  TO api;

GRANT ALL ON app.users_id_seq TO api;

ALTER TABLE app.users ENABLE ROW LEVEL SECURITY;

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
    id = app.current_user_id()
  )
  WITH CHECK (id = app.current_user_id());

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

CREATE FUNCTION app.cryptpassword()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    BEGIN
      IF tg_op = 'INSERT' OR NEW.password <> OLD.password THEN
        NEW.password = crypt(new.password, gen_salt('bf'));
      END IF;

      RETURN NEW;
    END;
  $$;

CREATE TRIGGER cryptpassword
  BEFORE INSERT OR UPDATE
  ON app.users
  FOR EACH ROW
    EXECUTE PROCEDURE app.cryptpassword();

COMMIT;

