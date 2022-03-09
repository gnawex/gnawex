-- Deploy gnawex:items to pg

BEGIN;

--------------------------------------------------------------------------------

CREATE TABLE app.items (
  item_id     BIGSERIAL PRIMARY KEY,
  name        TEXT NOT NULL,
  description TEXT NOT NULL,
  created_at  TIMESTAMP DEFAULT current_timestamp NOT NULL
);

GRANT
    SELECT,
    INSERT (name, description),
    UPDATE (name, description),
    DELETE
  ON TABLE app.items
  TO api;

GRANT SELECT ON TABLE app.items TO anon;
GRANT ALL ON TABLE app.items_item_id_seq TO api, verified_user;

--------------------------------------------------------------------------------

COMMIT;

