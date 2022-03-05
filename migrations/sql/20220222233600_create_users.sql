--------------------------------------------------------------------------------
-- `anon` permissions
-- NOTE: `anon` is pretty much public

CREATE ROLE anon;

GRANT USAGE ON SCHEMA public TO anon;

--------------------------------------------------------------------------------
-- `authenticator` serves as an alternative for `postgres` with less elevated
-- permissions. Use this to connect to the database, or something.

-- NOTE: I don't know how to set this for prod
-- TODO: Use passwd
CREATE ROLE authenticator NOINHERIT LOGIN PASSWORD 'foobarbaz';

GRANT anon TO authenticator;

--------------------------------------------------------------------------------

CREATE TYPE ROLE AS ENUM ('admin', 'mod', 'user');

-- TODO: FIgure out how to make the entire thing unqualified
-- CREATE SCHEMA gnawex;
-- ALTER DATABASE gnawex_db SET search_path TO gnawex;

-- TODO: Look into how to handle password encryption. Plain-text is fine while
-- there's no system in place (yet).
CREATE TABLE users (
  id              BIGSERIAL,
  hunter_id       BIGSERIAL UNIQUE,
  username        TEXT UNIQUE,
  password        TEXT,
  email           TEXT UNIQUE,
  role            ROLE,
  is_banned       BOOLEAN,
  is_verified     BOOLEAN,
  PRIMARY KEY(id)
);

