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

GRANT SELECT ON TABLE app.listings TO anon, verified_user;
GRANT SELECT, UPDATE ON TABLE app.listings TO gnawex_merchant;
GRANT SELECT, INSERT, UPDATE (is_active) ON TABLE app.listings TO api;
GRANT ALL ON app.listings_listing_id_seq TO verified_user;

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
    ON app.listings
    FOR EACH ROW
      EXECUTE PROCEDURE app.set_listing_user_id();

--------------------------------------------------------------------------------

CREATE FUNCTION app.adjust_listing()
  RETURNS TRIGGER
  LANGUAGE plpgsql
  AS $$
    DECLARE
      listing_remainder INTEGER;
    BEGIN
      IF NEW.cost > NEW.batch THEN
        SELECT mod(NEW.cost, NEW.batch) INTO listing_remainder;

        IF listing_remainder = 0 THEN
          NEW.quantity := NEW.quantity * NEW.batch;
          NEW.cost := NEW.cost / NEW.batch;
          NEW.batch := 1;
        END IF;

        RETURN NEW;
      ELSE
        SELECT mod(NEW.batch, NEW.cost) INTO listing_remainder;

        IF listing_remainder = 0 THEN
          NEW.batch := NEW.batch / NEW.cost;
          NEW.quantity := NEW.quantity * NEW.cost;
          NEW.cost := 1;
        END IF;

        RETURN NEW;
      END IF;
    END;
  $$;

COMMENT ON FUNCTION app.adjust_listing IS
  'Reduces needless batch sizes, and makes other adjustments accordingly.';

CREATE TRIGGER adjust_listing
  BEFORE INSERT
    ON app.listings
    FOR EACH ROW
      EXECUTE PROCEDURE app.adjust_listing();

GRANT EXECUTE ON FUNCTION app.adjust_listing TO verified_user;

--------------------------------------------------------------------------------

COMMIT;

