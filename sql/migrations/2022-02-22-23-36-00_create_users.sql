CREATE TYPE ROLE AS ENUM ('admin', 'mod', 'user');

CREATE TABLE users (
  id              BIGSERIAL,
  hunter_id       BIGSERIAL UNIQUE,
  username        VARCHAR(20) UNIQUE,
  password        VARCHAR(80),
  email           TEXT UNIQUE,
  role            ROLE,
  is_banned       BOOLEAN,
  is_verified     BOOLEAN,

  PRIMARY KEY(id)
);
