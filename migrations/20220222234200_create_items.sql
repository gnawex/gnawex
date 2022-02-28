CREATE TABLE items (
  id              BIGSERIAL,
  name            TEXT NOT NULL,
  description     TEXT NOT NULL,
  created_at      TIMESTAMP DEFAULT current_timestamp NOT NULL,
  updated_at      TIMESTAMP DEFAULT current_timestamp NOT NULL,

  PRIMARY KEY(id)
);
