CREATE TABLE items (
  id          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name        TEXT NOT NULL,
  wiki_link   TEXT NOT NULL,
  description TEXT NOT NULL,

  created_at  TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,
  updated_at  TIMESTAMPTZ,
  deleted_at  TIMESTAMPTZ
);

COMMENT ON COLUMN items.id          IS 'Tradable item ID';
COMMENT ON COLUMN items.name        IS 'Name of a tradable item';
COMMENT ON COLUMN items.wiki_link   IS 'MouseHunt Wiki link of the tradable item';
COMMENT ON COLUMN items.description IS 'Description of the tradable item';
COMMENT ON COLUMN items.created_at  IS 'When the tradable item was recorded in GNAWEX';
COMMENT ON COLUMN items.updated_at  IS 'When the tradable item record was updated in GNAWEX';
COMMENT ON COLUMN items.deleted_at  IS 'When the tradable item record was soft deleted';
