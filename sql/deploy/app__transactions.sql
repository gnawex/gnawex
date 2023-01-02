-- Deploy gnawex:transactions to pg
-- requires: listings

BEGIN;

--------------------------------------------------------------------------------

CREATE TYPE app.TRANSACTION_STATUS AS ENUM ('pending', 'completed', 'cancelled');

CREATE TABLE app.tradable_item_transactions (
    id                    UUID PRIMARY KEY DEFAULT gen_random_uuid()

    -- TODO: Check for listing type
  , buy_item_listing__id  BIGINT REFERENCES app.tradable_item_listings(id)
  , sell_item_listing__id BIGINT REFERENCES app.tradable_item_listings(id)

    -- Since we subtract the original `quantity` in the listing. Need this to
    -- keep a record so that we can sum all transactions which gives us the
    -- original quantity.
  , quantity              INT NOT NULL
  , status                app.TRANSACTION_STATUS NOT NULL

    -- This is here for convenience
    -- FIXME: Consider removing these two columns?
  , buyer__id             BIGINT REFERENCES app.users (id)
  , seller__id            BIGINT REFERENCES app.users (id)

  , created_at            TIMESTAMPTZ DEFAULT current_timestamp NOT NULL
  , updated_at            TIMESTAMPTZ DEFAULT current_timestamp
);

GRANT SELECT ON TABLE app.tradable_item_transactions TO api;
GRANT INSERT ON TABLE app.tradable_item_transactions TO gnawex_merchant;

--------------------------------------------------------------------------------

COMMIT;

