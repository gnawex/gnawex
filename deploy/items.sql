-- Deploy gnawex:items to pg

BEGIN;

CREATE TABLE app.items (
  item_id     BIGSERIAL PRIMARY KEY,
  name        TEXT NOT NULL,
  description TEXT NOT NULL,
  created_at  TIMESTAMP DEFAULT current_timestamp NOT NULL
);

COMMIT;
