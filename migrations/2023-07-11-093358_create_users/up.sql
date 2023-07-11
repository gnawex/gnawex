--------------------------------------------------------------------------------
-- Extensions

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

--------------------------------------------------------------------------------
-- Types

CREATE TYPE USER_ROLE AS ENUM
  ( 'verified_user'
  , 'verified_limited_user'
  , 'unverified_user'
  , 'banned_user'
  );

COMMENT ON TYPE USER_ROLE IS
  'Type of user';

--------------------------------------------------------------------------------
-- Tables

CREATE TABLE users (
  id        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  hunter_id BIGINT UNIQUE NOT NULL,
  username  CITEXT UNIQUE NOT NULL,

  -- 72 character limit due to 'bf' hash
  password  TEXT NOT NULL
              CHECK (char_length(password) <= 72
                AND char_length(password) >= 10),

  role      USER_ROLE NOT NULL
);

--------------------------------------------------------------------------------
-- Functions

CREATE FUNCTION current_user_id()
  RETURNS INTEGER
  LANGUAGE SQL
  AS $$
    SELECT NULLIF(current_setting('auth.user_id', TRUE), '') :: INTEGER
  $$;

CREATE FUNCTION cryptpassword()
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

--------------------------------------------------------------------------------
-- Triggers

CREATE TRIGGER cryptpassword
  BEFORE INSERT OR UPDATE
  ON users
  FOR EACH ROW
    EXECUTE PROCEDURE cryptpassword();
