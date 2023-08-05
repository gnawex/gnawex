-- Deploy gnawex:sessions to pg
-- requires: users

BEGIN;

SET LOCAL ROLE auth;

--------------------------------------------------------------------------------

CREATE TABLE auth.sessions (
  token TEXT NOT NULL PRIMARY KEY
        DEFAULT encode(gen_random_bytes(32), 'base64'),
  user_id INTEGER REFERENCES app.users (id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_on TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp() + '15min' :: INTERVAL,

  CHECK (expires_on > created_at)
);

COMMENT ON TABLE auth.sessions IS
  'User sessions';

COMMENT ON COLUMN auth.sessions.expires_on IS
  'Time on which the session expires';

CREATE INDEX ON auth.sessions (expires_on);

--------------------------------------------------------------------------------

CREATE VIEW auth.active_sessions AS
  SELECT
      token,
      user_id,
      created_at,
      expires_on
    FROM auth.sessions
    WHERE expires_on > clock_timestamp()
    WITH LOCAL CHECK OPTION;

COMMENT ON VIEW auth.active_sessions IS
  'A view for the sessions that are currently active';

--------------------------------------------------------------------------------
-- Functions

-- TODO: pgcron or some cron job
CREATE FUNCTION auth.clean_sessions()
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    DELETE FROM auth.sessions
      WHERE expires_on < clock_timestamp() - '1day' :: INTERVAL;
  $$;

COMMENT ON FUNCTION auth.clean_sessions IS
  'Cleans up sessions that have expired longer than a day ago';

--------------------------------------------------------------------------------

CREATE FUNCTION auth.login(username CITEXT, password TEXT)
  RETURNS TEXT
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    INSERT INTO auth.active_sessions(user_id)
      SELECT id
        FROM app.users
        WHERE username = login.username
          AND password = crypt(login.password, password)
      RETURNING token;
  $$;

COMMENT ON FUNCTION auth.login IS
  'Returns the token for a newly created session, or NULL on failure';

GRANT EXECUTE ON FUNCTION auth.login TO anon, api;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.refresh_session(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE sessions
      SET expires_on = DEFAULT
      WHERE token = session_token
        AND expires_on > clock_timestamp();
  $$;

COMMENT ON FUNCTION auth.refresh_session IS
  'Extend the expiration time of the given session';

GRANT EXECUTE ON FUNCTION auth.refresh_session TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.logout(session_token TEXT)
  RETURNS VOID
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    UPDATE auth.sessions
      SET expires_on = clock_timestamp()
      WHERE token = session_token;
  $$;

GRANT EXECUTE ON FUNCTION auth.logout TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.session_user_id(session_token TEXT)
  RETURNS BIGINT
  LANGUAGE sql
  SECURITY DEFINER
  AS $$
    SELECT user_id
    FROM auth.active_sessions
    WHERE token = session_token;
  $$;

GRANT EXECUTE ON FUNCTION auth.session_user_id TO anon;

--------------------------------------------------------------------------------

CREATE FUNCTION auth.authenticate()
  RETURNS VOID
  LANGUAGE plpgsql
  AS $$
    DECLARE
      session_token   TEXT;
      session_user_id INT;
    BEGIN
      SELECT current_setting('request.session_token', true)
        INTO session_token;

      RAISE LOG 'SESSION TOKEN: %', session_token;

      SELECT auth.session_user_id(session_token)
        INTO session_user_id;

      RAISE LOG 'SESSION USER ID: %', session_user_id;

      IF session_user_id IS NOT NULL THEN
        SET LOCAL ROLE verified_user;

        PERFORM set_config('auth.user_id', session_user_id :: TEXT, true);
      ELSE
        SET LOCAL ROLE anon;

        PERFORM set_config('auth.user_id', '', true);
      END IF;
    END;
  $$;

COMMENT ON FUNCTION auth.authenticate IS
  'Sets the role and user ID for the current transaction based on the cookie';

GRANT EXECUTE ON FUNCTION auth.authenticate TO anon;

--------------------------------------------------------------------------------

COMMIT;
