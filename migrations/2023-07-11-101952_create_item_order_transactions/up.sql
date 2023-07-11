CREATE TYPE ITEM_ORDER_TRANSACTION_STATUS AS ENUM ('pending', 'completed', 'cancelled');

CREATE TABLE item_order_transactions (
    id                    UUID PRIMARY KEY DEFAULT gen_random_uuid()

    -- TODO: Check for listing type
  , buy_item_order_id     BIGINT REFERENCES item_orders(id)
  , sell_item_order_id    BIGINT REFERENCES item_orders(id)

    -- Since we subtract the original `quantity` in the order. Need this to
    -- keep a record so that we can sum all transactions which gives us the
    -- original quantity.
  , quantity              INT NOT NULL
  , status                ITEM_ORDER_TRANSACTION_STATUS NOT NULL

  , buyer_id              BIGINT REFERENCES users (id)
  , seller_id             BIGINT REFERENCES users (id)

  , created_at            TIMESTAMPTZ DEFAULT current_timestamp NOT NULL
  , updated_at            TIMESTAMPTZ
);
