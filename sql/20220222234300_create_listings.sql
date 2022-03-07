--------------------------------------------------------------------------------
-- Types

CREATE TYPE LISTING_TYPE AS ENUM ('buy', 'sell');

--------------------------------------------------------------------------------
-- Tables

CREATE TABLE listings (
  -- TODO: I want to make this a UUID, not BIGSERIAL.
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
  PRIMARY KEY (id),
  FOREIGN KEY (item_id)
    REFERENCES items (id)
    ON DELETE CASCADE,
  FOREIGN KEY (user_id)
    REFERENCES users (id)
    ON DELETE CASCADE
);

CREATE TABLE transactions (
  id            UUID DEFAULT gen_random_uuid(),
  buy_order     BIGINT NOT NULL,
  sell_order    BIGINT NOT NULL,
  -- quantity      BIGINT NOT NULL,
  -- This is here for convenience
  buyer_id      BIGINT NOT NULL,
  seller_id     BIGINT NOT NULL,
  created_at    TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (buy_order)
    REFERENCES listings (id),
  FOREIGN KEY (sell_order)
    REFERENCES listings (id),
  FOREIGN KEY (buyer_id)
    REFERENCES users (id),
  FOREIGN KEY (seller_id)
    REFERENCES users (id)
);

-------------------------------------------------------------------------------
-- Indices

CREATE INDEX active_id ON listings (id) WHERE is_active = true;

--------------------------------------------------------------------------------
-- Views

--------------------------------------------------------------------------------
-- Functions

--------------------------------------------------------------------------------
-- Triggers

--------------------------------------------------------------------------------
-- Roles

--------------------------------------------------------------------------------
