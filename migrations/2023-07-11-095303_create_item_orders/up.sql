--------------------------------------------------------------------------------
-- Types

CREATE TYPE ORDER_KIND AS ENUM ('buy', 'sell');

--------------------------------------------------------------------------------
-- Tables

CREATE TABLE item_orders (
    id                     BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY

  -- Foreign Keys
  , item_id                BIGINT REFERENCES items (id)
  , user_id                BIGINT REFERENCES users (id)

  , kind                   ORDER_KIND NOT NULL
  , batched_by             SMALLINT NOT NULL CHECK (batched_by > 0)
  , unit_quantity          INT NOT NULL CHECK (unit_quantity > 0)
  , current_unit_quantity  INT NOT NULL CHECK (current_unit_quantity > 0)
  , cost                   INT NOT NULL CHECK (cost >= 0)

  -- Timestamps
  , created_at             TIMESTAMPTZ DEFAULT current_timestamp NOT NULL
  , updated_at             TIMESTAMPTZ
  , deactivated_at         TIMESTAMPTZ
);

--------------------------------------------------------------------------------
-- Functions

CREATE FUNCTION set_item_order_user_id()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    DECLARE
      current_user_id BIGINT;
    BEGIN
      RAISE LOG 'CURRENT_ROLE: %', current_user;

      SELECT current_user_id() INTO current_user_id;

      IF current_user_id IS NOT NULL THEN
        NEW.user__id := current_user_id;

        RETURN NEW;
      ELSE
        RAISE INSUFFICIENT_PRIVILEGE
          USING detail = 'current_user_id is NULL';
      END IF;
    END;
  $$;

COMMENT ON FUNCTION set_item_order_user_id IS
  'Updates the item order row by setting its user ID with the owner user ID';

CREATE FUNCTION adjust_item_order()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    DECLARE
      divisor INTEGER;
    BEGIN
      SELECT gcd(NEW.cost, NEW.batched_by) INTO divisor;

      NEW.unit_quantity := NEW.unit_quantity * divisor;
      NEW.current_unit_quantity := NEW.current_unit_quantity * divisor;
      NEW.cost := NEW.cost / divisor;
      NEW.batched_by := NEW.batched_by / divisor;

      RETURN NEW;
    END;
  $$;

COMMENT ON FUNCTION adjust_item_order IS
  'Normalizes the item order parameters to have more consistent matching in the future';

--------------------------------------------------------------------------------
-- Triggers

CREATE TRIGGER set_item_order_user_id
  BEFORE INSERT
    ON item_orders
    FOR EACH ROW
      EXECUTE PROCEDURE set_item_order_user_id();

-- COMMENT ON TRIGGER set_item_order_user_id IS
--   'Triggers `set_item_order_user_id()` before the row gets inserted to `item_orders`';

CREATE TRIGGER normalize_item_order
  BEFORE INSERT
    ON item_orders
    FOR EACH ROW
      EXECUTE PROCEDURE adjust_item_order();

-- COMMENT ON TRIGGER normalize_item_order IS
--   'Triggers `adjust_item_order()` before the row gets inserted to `item_orders`';
