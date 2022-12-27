-- Deploy gnawex:items to pg

BEGIN;

-------------------------------------------------------------------------------

CREATE TABLE app.tradable_items (
  id          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name        TEXT NOT NULL,
  wiki_link   TEXT NOT NULL,
  description TEXT NOT NULL,

  created_at  TIMESTAMP DEFAULT current_timestamp NOT NULL,
  updated_at  TIMESTAMP DEFAULT current_timestamp,
  deleted_at  TIMESTAMP DEFAULT current_timestamp
);

GRANT
    SELECT,
    INSERT (name, wiki_link, description),
    UPDATE (name, wiki_link, description),
    DELETE
  ON TABLE app.tradable_items
  TO api;

GRANT SELECT ON TABLE app.tradable_items TO anon, verified_user;
GRANT ALL ON TABLE app.tradable_items_id_seq TO api, verified_user;

-------------------------------------------------------------------------------
-- Comments

COMMENT ON COLUMN app.tradable_items.id IS 'Tradable item ID';
COMMENT ON COLUMN app.tradable_items.name IS 'Name of a tradable item';
COMMENT ON COLUMN app.tradable_items.wiki_link IS 'MouseHunt Wiki link of the tradable item';
COMMENT ON COLUMN app.tradable_items.description IS 'Description of the tradable item';
COMMENT ON COLUMN app.tradable_items.created_at IS 'When the tradable item was recorded in GNAWEX';
COMMENT ON COLUMN app.tradable_items.updated_at IS 'When the tradable item record was updated in GNAWEX';
COMMENT ON COLUMN app.tradable_items.deleted_at IS 'When the tradable item record was soft deleted';

-------------------------------------------------------------------------------

COMMIT;

