-- Ensure the DB defaults to UTC
SET timezone TO 'UTC';

CREATE TYPE ROLE AS ENUM ('admin', 'mod', 'user');

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
