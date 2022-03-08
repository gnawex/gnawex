-- Deploy gnawex:users to pg
-- requires: app_schema
-- requires: pgcrypto
-- requires: citext

BEGIN;

CREATE TYPE USER_ROLE AS ENUM ('verified_user', 'unverified_user', 'banned_user');
COMMENT ON TYPE USER_ROLE IS
  'Type of user';

CREATE TABLE app.users (
  user_id   BIGSERIAL PRIMARY KEY,
  hunter_id BIGSERIAL UNIQUE NOT NULL,
  username  CITEXT UNIQUE NOT NULL,
  -- 72 character limit due to 'bf' hash
  password  TEXT NOT NULL
    CHECK (
      char_length(password) <= 72 AND char_length(password) >= 10
    ),

  role      USER_ROLE NOT NULL
);

ALTER TABLE app.users ENABLE ROW LEVEL SECURITY;

COMMENT ON TABLE app.users IS
  'GNAWEX users';

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

