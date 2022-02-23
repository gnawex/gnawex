CREATE TYPE LISTING_TYPE AS ENUM ('buy', 'sell');

CREATE TABLE listings (
  id              BIGSERIAL NOT NULL,
  item_id         BIGSERIAL NOT NULL,
  user_id         BIGSERIAL NOT NULL,
  quantity        INT NOT NULL,
  cost            INT NOT NULL CHECK (cost > 0),
  type            LISTING_TYPE NOT NULL,
  batch           INT NOT NULL,
  is_active       BOOLEAN DEFAULT true NOT NULL,

  created_at      TIMESTAMP DEFAULT current_timestamp NOT NULL,
  updated_at      TIMESTAMP DEFAULT current_timestamp NOT NULL,

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
  created_at    TIMESTAMP DEFAULT current_timestamp NOT NULL,

  PRIMARY KEY (id)
);

ALTER SEQUENCE items_id_seq RESTART;
ALTER SEQUENCE listings_id_seq RESTART;
ALTER SEQUENCE listings_item_id_seq RESTART;
ALTER SEQUENCE users_hunter_id_seq RESTART;
ALTER SEQUENCE users_id_seq RESTART;

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
