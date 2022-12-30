-- Deploy gnawex:listings to pg
-- requires: items
-- requires: users

BEGIN;

CREATE TYPE app.LISTING_TYPE AS ENUM ('buy', 'sell');

CREATE TABLE app.tradable_item_listings (
  id                 BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,

  -- Foreign Keys
  tradable_item__id  BIGINT REFERENCES app.tradable_items (id),
  user_id            BIGINT REFERENCES app.users (user_id),

  type               app.LISTING_TYPE NOT NULL,
  batched_by         SMALLINT NOT NULL,
  unit_quantity      INT NOT NULL,
  cost               BIGINT NOT NULL CHECK (cost >= 0),
  active             BOOLEAN DEFAULT true NOT NULL,

  -- Timestamps
  created_at         TIMESTAMPTZ DEFAULT current_timestamp NOT NULL,
  updated_at         TIMESTAMPTZ
);

COMMENT ON TABLE app.tradable_item_listings IS
  'A buy/sell listing to be matched by with another by GNAWEX';

CREATE INDEX active_id
  ON app.tradable_item_listings (tradable_item__id)
  WHERE active = true;

GRANT SELECT ON TABLE app.tradable_item_listings TO anon, verified_user;
GRANT SELECT, UPDATE ON TABLE app.tradable_item_listings TO gnawex_merchant;
GRANT SELECT, INSERT, UPDATE (active) ON TABLE app.tradable_item_listings TO api;
GRANT ALL ON app.tradable_item_listings_id_seq TO verified_user;

--------------------------------------------------------------------------------

CREATE FUNCTION app.set_listing_user_id()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    DECLARE
      current_user_id BIGINT;
    BEGIN
      RAISE LOG 'CURRENT_ROLE: %', current_user;

      SELECT app.current_user_id() INTO current_user_id;

      IF current_user_id IS NOT NULL THEN
        NEW.user_id := current_user_id;

        RETURN NEW;
      ELSE
        RAISE INSUFFICIENT_PRIVILEGE
          USING detail = 'current_user_id is NULL';
      END IF;
    END;
  $$;

CREATE TRIGGER set_listing_user_id
  BEFORE INSERT
    ON app.tradable_item_listings
    FOR EACH ROW
      EXECUTE PROCEDURE app.set_listing_user_id();

--------------------------------------------------------------------------------

CREATE FUNCTION app.adjust_listing()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    DECLARE
      divisor INTEGER;
    BEGIN
      SELECT gcd(NEW.cost, NEW.batched_by) INTO divisor;

      NEW.unit_quantity := NEW.unit_quantity * divisor;
      NEW.cost := NEW.cost / divisor;
      NEW.batched_by := NEW.batched_by / divisor;

      RETURN NEW;
    END;
  $$;

COMMENT ON FUNCTION app.adjust_listing IS
  'Reduces needless batch sizes, and makes other adjustments accordingly.';

CREATE TRIGGER adjust_listing
  BEFORE INSERT
    ON app.tradable_item_listings
    FOR EACH ROW
      EXECUTE PROCEDURE app.adjust_listing();

GRANT EXECUTE ON FUNCTION app.adjust_listing TO verified_user;

--------------------------------------------------------------------------------

COMMIT;

