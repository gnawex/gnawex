--------------------------------------------------------------------------------
-- Types

CREATE TYPE LISTING_TYPE AS ENUM ('buy', 'sell');

--------------------------------------------------------------------------------
-- Tables

CREATE TABLE listings (
  id              BIGSERIAL NOT NULL,
  item_id         BIGSERIAL NOT NULL,
  user_id         BIGSERIAL NOT NULL,
  quantity        INT NOT NULL,
  cost            INT NOT NULL CHECK (cost >= 0),
  type            LISTING_TYPE NOT NULL,
  batch           INT NOT NULL,
  is_active       BOOLEAN DEFAULT true NOT NULL,

  created_at      TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,
  updated_at      TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,

  PRIMARY KEY(id),

  FOREIGN KEY(item_id)
    REFERENCES items(id)
    ON DELETE CASCADE,

  FOREIGN KEY(user_id)
    REFERENCES users(id)
    ON DELETE CASCADE
);

CREATE TABLE transactions (
  id            UUID DEFAULT gen_random_uuid(),
  buy_order     BIGSERIAL NOT NULL,
  sell_order    BIGSERIAL NOT NULL,
  created_at    TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,

  PRIMARY KEY (id)
);

-------------------------------------------------------------------------------
-- Indices

CREATE INDEX active_id ON listings (id) WHERE is_active = true;

-------------------------------------------------------------------------------
-- Views

-- TODO: Not important, can delete
CREATE MATERIALIZED VIEW users_listings AS
  (
      SELECT
          listings.id AS listing_id,
          users.username AS owner,
          listings.item_id,
          items.name,
          listings.type,
          listings.quantity,
          listings.cost,
          (listings.quantity * listings.cost) AS total_cost
      FROM users
      JOIN listings ON users.id = listings.user_id
      JOIN items ON items.id = listings.item_id
      WHERE users.is_banned = false AND users.is_verified = true
  );

--------------------------------------------------------------------------------
-- Functions

-- Validates if the user has permission to insert.
-- TODO: Maybe I can somehow use row level security for this?
CREATE OR REPLACE FUNCTION validate_users_insert() RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    SELECT 1
      FROM users
      WHERE users.id = NEW.user_id
        AND users.is_banned   = false
        AND users.is_verified = true
  ) THEN
    RAISE NOTICE 'User % is OK', NEW.user_id;

    -- Not gonna change anything so we can insert as-is.
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'User % is not permitted to create a listing', NEW.user_id;
  END IF;
END;
$$
LANGUAGE plpgsql;

--------------------------------------------------------------------------------
-- Triggers

CREATE OR REPLACE TRIGGER validate_user_before_insert
  BEFORE INSERT
  ON listings
  FOR EACH ROW
  EXECUTE PROCEDURE validate_users_insert();

--------------------------------------------------------------------------------

