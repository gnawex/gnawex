-- Deploy gnawex:sessions to pg
-- requires: users

BEGIN;

SET LOCAL ROLE auth;

--------------------------------------------------------------------------------
-- Tables

CREATE TABLE auth.sessions (
  token TEXT NOT NULL PRIMARY KEY
        DEFAULT encode(gen_random_bytes(32), 'base64'),
  user_id INTEGER REFERENCES app.users (user_id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_on TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp() + '15min' :: INTERVAL,

  CHECK (expires_on > created_at)
);

CREATE INDEX ON auth.sessions (expires_on);

--------------------------------------------------------------------------------
-- Views

CREATE VIEW auth.active_sessions AS
  SELECT
      token,
      user_id,
      created_at,
      expires_on
    FROM auth.sessions
    WHERE expires_on > clock_timestamp()
    WITH LOCAL CHECK OPTION;

--------------------------------------------------------------------------------
-- Functions

CREATE FUNCTION auth.clean_sessions()
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    DELETE FROM auth.sessions
      WHERE expires_on < clock_timestamp() - '1day' :: INTERVAL;
  $$;

CREATE FUNCTION auth.login(username CITEXT, password TEXT)
  RETURNS TEXT
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    INSERT INTO auth.active_sessions(user_id)
      SELECT user_id
      FROM app.users
      WHERE username = login.username
        AND password = crypt(login.password, password)
      RETURNING token;
  $$;

CREATE FUNCTION auth.refresh_session(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE sessions
    SET expires_on = DEFAULT
    WHERE token = session_token AND expires_on > clock_timestamp();
  $$;

CREATE FUNCTION auth.logout(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE auth.sessions
    SET expires_on = clock_timestamp()
    WHERE token = session_token;
  $$;

CREATE FUNCTION auth.session_user_id(session_token TEXT)
  RETURNS BIGINT
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    SELECT user_id
    FROM auth.active_sessions
    WHERE token = session_token;
  $$;

CREATE FUNCTION auth.authenticate()
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token   TEXT;
      session_user_id INT;
    BEGIN
      SELECT current_setting('request.cookies', TRUE)::json->>'session_token'
        INTO session_token;

      SELECT auth.session_user_id(session_token)
        INTO session_user_id;

      IF session_user_id IS NOT NULL THEN
        SET LOCAL ROLE verified_user;

        PERFORM set_config('auth.user_id', session_user_id :: TEXT, TRUE);
      ELSE
        SET LOCAL ROLE anon;

        PERFORM set_config('auth.user_id', '', TRUE);
      END IF;
    END;
  $$;

--------------------------------------------------------------------------------
-- Comments

COMMENT ON TABLE auth.sessions IS
  'User sessions';

COMMENT ON COLUMN auth.sessions.expires_on IS
  'Time on which the session expires';

COMMENT ON FUNCTION auth.clean_sessions IS
  'Cleans up sessions that have expired longer than a day ago';

COMMENT ON FUNCTION auth.login IS
  'Returns the token for a newly created session, or NULL on failure';

COMMENT ON FUNCTION auth.refresh_session IS
  'Extend the expiration time of the given session';

COMMENT ON FUNCTION auth.authenticate IS
  'Sets the role and user ID for the current transaction based on the cookie';

--------------------------------------------------------------------------------

COMMIT;
