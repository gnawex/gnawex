-- Deploy gnawex:listings to pg
-- requires: items
-- requires: users

BEGIN;

CREATE TYPE app.LISTING_TYPE AS ENUM ('buy', 'sell');

CREATE TABLE app.listings (
  listing_id BIGSERIAL PRIMARY KEY,
  item_id    BIGINT REFERENCES app.items (item_id),
  user_id    BIGINT REFERENCES app.users (user_id),

  quantity   INT NOT NULL,
  cost       BIGINT NOT NULL CHECK (cost >= 0),
  type       app.LISTING_TYPE NOT NULL,
  batch      INT NOT NULL,
  is_active  BOOLEAN DEFAULT true NOT NULL,

  created_at TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,
  updated_at TIMESTAMPTZ DEFAULT current_timestamp NOT NULL
);

COMMENT ON TABLE app.listings IS
  'A buy/sell listing to be matched by with another by GNAWEX';

CREATE INDEX active_id ON app.listings (item_id) WHERE is_active = true;

COMMIT;

