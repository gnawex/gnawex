-- Deploy gnawex:transactions to pg
-- requires: listings

BEGIN;

CREATE TABLE app.transactions (
  transaction_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  buy_order      BIGINT REFERENCES app.listings(listing_id),
  sell_order     BIGINT REFERENCES app.listings(listing_id),

  -- Since we subtract the original `quantity` in the listing. Need this to
  -- keep a record so that we can sum all transactions which gives us the
  -- original quantity.
  quantity       BIGINT NOT NULL,

  -- This is here for convenience
  buyer_id       BIGINT REFERENCES app.users (user_id),
  seller_id      BIGINT REFERENCES app.users (user_id),

  created_at     TIMESTAMPTZ DEFAULT current_timestamp NOT NULL
);

COMMIT;
