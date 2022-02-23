CREATE TYPE LISTING_TYPE AS ENUM ('buy', 'sell');

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

-------------------------------------------------------------------------------
-- ACTIONS
--
-- 1. match/1
-- 2. transact/3

CREATE OR REPLACE FUNCTION match() RETURNS trigger AS $$
DECLARE
  matched_id INTEGER;
BEGIN
  CASE
    -- If it's a BUY listing, look for SELL listings to match with.
    WHEN NEW.type = 'buy' THEN
      SELECT listings.id
        FROM listings
        WHERE listings.item_id = NEW.item_id
          AND listings.type = 'sell'
          AND listings.is_active = true
          AND listings.quantity > 0
          AND listings.cost = NEW.cost
        ORDER BY created_at ASC
        LIMIT 1
        INTO matched_id;

      IF matched_id IS NOT NULL THEN
        RAISE NOTICE 'I found a matching SELL listing (id: %)', matched_id;
        CALL transact(NEW.id, matched_id, NEW.item_id);
      ELSE
        RAISE NOTICE 'Could not find a SELL order for listing ID % at %.', NEW.id, now();
      END IF;

    -- If it's a SELL listing, look for BUY listings to match with.
    WHEN NEW.type = 'sell' THEN
      SELECT listings.id
        FROM listings
        WHERE listings.item_id = NEW.item_id
          AND listings.type = 'buy'
          AND listings.is_active = true
          AND listings.quantity > 0
          AND listings.cost = NEW.cost
        ORDER BY created_at ASC
        LIMIT 1
        INTO matched_id;

      IF matched_id IS NOT NULL THEN
        RAISE NOTICE 'I found a matching BUY listing (id: %)', matched_id;
        CALL transact(matched_id, NEW.id, NEW.item_id);
      ELSE
        RAISE NOTICE 'Could not find a BUY order for listing ID % at %.', NEW.id, now();
      END IF;
  END CASE;

  RETURN NULL;
END;
$$
LANGUAGE plpgsql;

-- Validates if the user has permission to insert.
CREATE OR REPLACE FUNCTION validate_users_insert() RETURNS trigger AS $$
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

CREATE OR REPLACE TRIGGER match_listings
  AFTER INSERT
  ON listings
  FOR EACH ROW
  EXECUTE PROCEDURE match();

CREATE OR REPLACE TRIGGER validate_user_before_insert
  BEFORE INSERT
  ON listings
  FOR EACH ROW
  EXECUTE PROCEDURE validate_users_insert();

-- The `transact` procedure processes two listings (or orders).
-- `transact` requires these two listings to have the same `item_id` value.
--
-- 1. Creates a row in `transactions` with the buy and sell order IDs.
-- 2. Gets the sell order's quantity; puts it in `sell_quantity`.
-- 3. Gets the buy order's quantity; puts it in `buy_quantity`.
-- 4. Attempts to perform an update on the buy order.
-- 5. Attempts to perform an update on the sell order.
--
-- It's considered a failure if the update operations in step 4 and 5 affects 0
-- rows. Such failure will cause a rollback in the database transaction.
--
-- Example:
--
-- Buy order ID 3, sell order ID 1, item ID 1
-- db=# CALL transact(3, 1, 1);
CREATE OR REPLACE PROCEDURE transact(buy_order_id BIGINT, sell_order_id BIGINT,
listing_item_id BIGINT) AS $$
DECLARE
  -- Number of affected rows in the buy order's update
  buy_affected_rows  INTEGER;

  -- Number of affected rows in the sell order's update
  sell_affected_rows INTEGER;

  -- Item quantity of the buy order
  buy_quantity       INTEGER;

  -- Item quantity of the sell order
  sell_quantity      INTEGER;
BEGIN
  RAISE NOTICE 'Creating a transaction for BUY order %, and SELL order % at %.',
    buy_order_id, sell_order_id, now();

  -- Creates a transaction involving two listings; a buy and sell listing.
  INSERT 
    INTO transactions (buy_order, sell_order)
    VALUES (buy_order_id, sell_order_id);

  -- Get the sell order's quantity
  SELECT COALESCE(SUM(quantity), 0) AS quantity
    FROM listings
    WHERE listings.id        = sell_order_id
      AND listings.type      = 'sell'
      AND listings.is_active = true
    INTO sell_quantity;

  IF sell_quantity = 0 THEN
    RAISE EXCEPTION 'Sell order is not valid. It either has no stock or is inactive.';
  END IF;

  -- Get the buy order's quantity
  SELECT COALESCE(SUM(quantity), 0) AS quantity
    FROM listings
    WHERE listings.id        = buy_order_id
      AND listings.type      = 'buy'
      AND listings.is_active = true
    INTO buy_quantity;

  IF sell_quantity = 0 THEN
    RAISE EXCEPTION 'Buy order is not valid. It either has no stock or is inactive.';
  END IF;

  -- Reduces the quantity of a listing by 1. This ensures that
  -- something was actually reduced cause otherwise, the transaction
  -- just rolls back. It's considered an invalid transaction.
  WITH affected_buy_cte AS (
      UPDATE listings
      SET quantity = GREATEST(quantity - sell_quantity, 0),
          is_active =
            CASE WHEN GREATEST(quantity - sell_quantity, 0) = 0
                 THEN false
                 ELSE true
            END
      WHERE id = buy_order_id
        AND listings.type = 'buy'
        AND listings.quantity > 0
        AND listings.item_id = listing_item_id
      RETURNING 1
  )
  SELECT COUNT(*) FROM affected_buy_cte INTO buy_affected_rows;

  IF buy_affected_rows = 0 THEN
    RAISE EXCEPTION 'I was not able to update the BUY order. This transaction is not valid.';
  END IF;

  -- This is pretty much the same as the previous query except for the
  -- sell order listing.
  WITH affected_sell_cte AS (
      UPDATE listings
      SET quantity = GREATEST(quantity - buy_quantity, 0),
          is_active =
            CASE WHEN GREATEST(quantity - buy_quantity, 0) = 0
                 THEN false
                 ELSE true
            END
      WHERE id = sell_order_id 
        AND listings.type = 'sell'
        AND listings.item_id = listing_item_id
      RETURNING 1
  )
  SELECT COUNT(*) FROM affected_sell_cte INTO sell_affected_rows;

  IF sell_affected_rows = 0 THEN
    RAISE EXCEPTION 'I was not able to update the SELL order. This transaction is not valid.';
  END IF;
END;
$$
LANGUAGE plpgsql;

